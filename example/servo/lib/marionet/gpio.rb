module MarioNet
  class GPIO

    def initialize(task)
      @task = task
    end
    
    def digital_write(pin, val, &block)
      @task.send_command({command:"gpio_digital_write", args:[pin, val]},
                         &block)
    end

    def digital_read(pin, &block)
      @task.send_command({command:"gpio_digital_read", args:[pin]}, 
                         &block)
    end

  end
end
