class Pt
  attr_accessor :x, :y
  def distToOrigin
    Math.sqrt(x * x + y * y)
  end
end

class ColorPt < Pt
  attr_accessor :color
  def darken
    self.color = "dark " + self.color
  end
end

class Pt3D < Pt
  attr_accessor :z
  def distToOrigin
    Math.sqrt(x * x + y * y + z * z)
  end
end

# this does not exist in Ruby
#class ColorPt3D < ColorPt, Pt3D
#... 
#end

class ColorPt3D_1 < ColorPt
  attr_accessor :z
  def distToOrigin
    Math.sqrt(x * x + y * y + z * z)
  end
end

class ColorPt3D_2 < Pt3D
  attr_accessor :color
  def darken
    self.color = "dark " + self.color
  end
end
