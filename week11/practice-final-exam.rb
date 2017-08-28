class A
  def m1
    self.m2()
  end
  def m2
    puts "m2() A"
  end
end
module M
  def m3
    self.m4()
  end
end
class B < A
  def m2
    puts "m2() B"
  end
end
class C < A
  include M
  def m4
    puts "m4 C"
  end
end
class D < B
  include M
end
