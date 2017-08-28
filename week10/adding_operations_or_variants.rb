class Exp

end

class Value < Exp

end

class Int < Value
  attr_reader :i

  def initialize i
    @i = i
  end

  def eval
    self
  end

  def toString
    @i.to_s
  end

  def hasZero
    i == 0
  end

  def noNegConstants
    if i < 0
      Negate.new(Int.new(-@i))
    else
      self
    end
  end
end

class Negate < Exp
  attr_reader :e

  def initialize e
    @e = e
  end

  def eval
    Int.new(-@e.eval.i) # error if e.eval has no i method
  end

  def toString
    "-(" + @e.toString + ")"
  end

  def hasZero
    @e.hasZero
  end

  def noNegConstants
    Negate.new(@e.noNegConstants)
  end
end

class Add < Exp
  attr_reader :e1, :e2

  def initialize (e1,e2)
    @e1 = e1
    @e2 = e2
  end

  def eval
    Int.new(@e1.eval.i + @e2.eval.i)
  end

  def toString
    "(" + @e1.toString + "+" + @e2.toString + ")"
  end

  def hasZero
    @e1.hasZero || @e2.hasZero
  end

  def noNegConstants
    Add.new(@e1.noNegConstants, @e2.noNegConstants)
  end
end

class Mult < Exp
  attr_reader :e1, :e2

  def initialize (e1,e2)
    @e1 = e1
    @e2 = e2
  end

  def eval
    Int.new(@e1.eval.i * @e2.eval.i)
  end

  def toString
    "(" + @e1.toString + "*" + @e2.toString + ")"
  end

  def hasZero
    @e1.hasZero || @e2.hasZero
  end

  def noNegConstants
    Mult.new(@e1.noNegConstants, @e2.noNegConstants)
  end
end
