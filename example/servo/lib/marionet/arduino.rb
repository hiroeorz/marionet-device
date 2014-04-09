module MarioNet
  class Arduino

    def initialize(task)
      @task = task
    end
    
    def analog_write(pin, val, &block)
      @task.send_command({command:"arduino_analog_write", args:[pin, val]},
                         &block)
    end

  end

  class CommandError < StandardError
  end

  class TooMuchTaskError < StandardError
  end
end
