STATUSES = [ :online, :offline ]

MAX_SECONDS_TO_SLEEP = 4000

class Node
  attr_accessor :status

  def initialize buddies
  end

  def run
    
  end

  def notify_presence status, buddies

  end

  # Made so statuses can be updated easier.
  def update_status
    self.status = STATUSES[rand(STATUSES.length)] if self.status.nil?
    self.status = (STATUSES.index(self.status) + 1) % STATUSES.length
  end

end

class Simulator
  def initialize num_nodes, num_buddies
    @num_nodes = num_nodes
    @num_buddies = num_buddies
  end

  def start_nodes
    @num_nodes.times.each do |i|
      Thread.new do 
        node = Node.new @num_buddies
        loop do
          node.update_status
          sleep rand(MAX_SECONDS_TO_SLEEP)
        end
      end

      #@nodes << node_thread
    end

  end

  def fail_send?

  end

  def update_presence client, status

  end
end

simulator = Simulator.new 1000, 10
