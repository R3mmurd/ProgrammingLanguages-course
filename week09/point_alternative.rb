class Point
  attr_accessor :x, :y # defines methods x, y, x=, y=

  def initialize(x,y)
    @x = x
    @y = y
  end

  def distFromOrigin
    Math.sqrt(@x * @x + @y * @y) # uses instance variables
  end
  
  def distFromOrigin2
    Math.sqrt(x * x + y * y) # uses getter methods
  end

end

class Point
  attr_accessor :color # color and color=

  def initialize(x,y,c="clear")
    @x = x
    @y = y
    @color = c
  end
end
