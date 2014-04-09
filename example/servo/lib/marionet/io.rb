module MarioNet
  class IO

    # URI to ZeroMQ binded port.
    ZMQ_SUB_URI = "tcp://127.0.0.1:6788"
    ZMQ_REQ_URI = "tcp://127.0.0.1:6789"

    # Error raised if task count over TASK_COUNT_LIMIT.
    TASK_COUNT_LIMIT = 30

    def initialize
      @req = nil
      @callback = nil
    end

    def task_queue
      @task_queue ||= []
    end

    def remote_events
      @remote_events ||= {}
    end

    def context
      @sub_context ||= EM::ZeroMQ::Context.new(1)
    end

    def callback
      @callback ||= {}
    end

    def remote(ev, &block)
      raise ArgumentError.new("Block must given!") unless block_given?
      remote_events[ev] = block
    end

    def call(ev)
      return unless ev.kind_of?(Hash)
      type   = ev["type"]
      type = "analog"  if type == "ai" # delete when source data type fixed.
      type = "digital" if type == "di" # delete when source data type fixed.

      device = ev["id"]
      no     = ev["no"]
      val    = ev["val"]
      opts   = ev["opts"]

      task1 = remote_events[{:device => device, :type => type, :no => no}]
      task1.call(no, val, ev) if task1

      task2 = remote_events[{:device => device, :type => type}]
      task2.call(no, val, ev) if task2
    end

    def send_command(obj, &block)
      exec_block(obj, block)
    end

    def exec_block(obj, block)
      command_uuid = uuid
      obj[:uuid] = command_uuid

      if @req.send_msg(obj.to_json)
        callback[command_uuid] = block if block
      else
        task_queue << {:obj => obj, :block => block}
        if TASK_COUNT_LIMIT < task_queue.length
          raise TooMuchTaskError.new("too much task: #{task_queue.length}")
        end
      end
    end

    def flash_task!
      return if task_queue.empty?
      task = task_queue.shift
      obj = task[:obj]
      block = task[:block]
      exec_block(obj, block)
    end

    def start
      EM.run {
        print "connecting to request port ( #{ZMQ_REQ_URI} )..."
        @req = context.socket(ZMQ::REQ)
        @req.connect(ZMQ_REQ_URI); puts "done"

        print "connecting to subscribe port ( #{ZMQ_SUB_URI} )..."
        sub = context.socket(ZMQ::SUB)
        sub.connect(ZMQ_SUB_URI); puts "done"
        sub.subscribe
        
        @req.on(:message) { |part|
          if !callback.empty?
            result = JSON.parse(part.copy_out_string)

            if result["error"]
              raise MarioNet::CommandError.new(result["error"])
            else
              fun = callback.delete(result["uuid"])
              fun.call(result["return"]) if fun
            end
          end

          flash_task!
        }

        sub.on(:message) { |part|
          self.call(JSON.parse(part.copy_out_string))
        }
      }
    end

    private

    def uuid
      UUIDTools::UUID.random_create.to_s
    end

  end
end
