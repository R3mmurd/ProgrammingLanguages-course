class A
  attr_accessor :x
  def m1
    @x = 4
  end
  def m2
    m1
    @x > 4
  end
  def m3
    @x = 4
    @x > 4
  end
  def m4
    self.x = 4
    @x > 4
  end
end

class MyRange
  include Enumerable
  def initialize(low,high)
    @low = low
    @high = high
  end
  def each
    i=@low
    while i <= @high
      yield i
      i=i+1
    end
  end
end

class X
  def initialize a
    @arr = a
  end
  def get i
    @arr[i]
  end
  def sum
    @arr.inject(0) {|acc,x| acc + x}
  end
end

class Y < X
  def initialize a
    super
    @ans = false
  end
  def sum
    if !@ans
      @ans = @arr.inject(0) {|acc,x| acc + x}
    end
    @ans
  end
end
