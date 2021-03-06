# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       for being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue 
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
      (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2) 
      real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
      if real_close(x1,x2)
        VerticalLine.new x1
      else
        m = (y2 - y1).to_f / (x2 - x1)
        b = y1 - m * x1
        Line.new(m,b)
      end
  end
  
  def inbetween(v,end1,end2)
    if v
      (end1 - GeometryExpression::Epsilon <= v && v <= end2 + GeometryExpression::Epsilon) ||
      (end2- GeometryExpression::Epsilon <= v && v <= end1 + GeometryExpression::Epsilon)
    else 
      false 
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  def preprocess_prog
    self # no pre-processing to do here
  end

  def eval_prog env 
    self 
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env 
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y
  def initialize(x,y)
    @x = x
    @y = y
  end
  def shift(dx,dy)
    Point.new(@x + dx, @y + dy)
  end
  def intersect other
    other.intersectPoint self 
  end
  def intersectPoint p
    if real_close_point(@x,@y,p.x,p.y)
      self
    else 
      NoPoints.new()
    end
  end
  def intersectLine line
    if real_close(@x*line.m + line.b,@y)
      self
    else 
      NoPoints.new()
    end
  end
  def intersectVerticalLine vline
    if real_close(@x,vline.x)
      self
    else 
      NoPoints.new()
    end
  end
  def intersectWithSegmentAsLineResult seg
    line = two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2)
    if inbetween(@x,seg.x1,seg.x2) && inbetween(@y,seg.y1,seg.y2)
      line.intersectPoint self 
    else 
      NoPoints.new()
    end
  end
end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 
  def initialize(m,b)
    @m = m
    @b = b
  end
  def shift(dx,dy)
    Line.new(@m,@b + dy - (@m * dx))
  end
  def intersect other
    other.intersectLine self 
  end
  def intersectPoint p
    if real_close(p.y,@m*p.x + @b)
      p
    else 
      NoPoints.new()
    end
  end
  def intersectLine line
    if real_close(@m,line.m)
      if real_close(@b,line.b)
        #Same line
        self
      else 
        #Same slope (parallel lines)
        NoPoints.new()
      end
    else 
      #Check for single point
      x = (@b - line.b) / (line.m - @m)
      y = @m * x + @b
      Point.new(x,y)
    end
  end
  def intersectVerticalLine vline
    Point.new(vline.x,@m*vline.x + @b)
  end
  def intersectWithSegmentAsLineResult seg
    line = two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2)
    #Vertical line seg
    if real_close(seg.x1,seg.x2)
      y1 = @m*seg.x1 + @b
      if inbetween(y1, seg.y1,seg.y2)
        #intersect point 
        Point.new(seg.x1,y1)
      else 
        NoPoints.new()
      end
    elsif real_close(@m,line.m)
      if real_close(@b,line.b)
        #same line
        seg
      else 
        #parallel
        NoPoints.new()
      end
    else 
      #see if point is in segment
      x1 = (line.b - @b) / (@m - line.m)
      y1 = @m * x1 + @b
      if inbetween(x1,seg.x1,seg.x2) && inbetween(y1,seg.y1,seg.y2)
        Point.new(x1,y1)
      else
        NoPoints.new()
      end
    end
  end
end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end
  def shift(dx,dy)
    VerticalLine.new(@x + dx)
  end
  def intersect other
    other.intersectVerticalLine self 
  end
  def intersectPoint p
    if real_close(@x,p.x)
      p
    else 
      NoPoints.new()
    end
  end
  def intersectLine line
    Point.new(@x,line.m*@x + line.b)
  end
  def intersectVerticalLine vline
    if real_close(@x, vline.x)
      #Same line
      self
    else 
      #Parallel
      NoPoints.new()
    end
  end
  def intersectWithSegmentAsLineResult seg
    line = two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2)
    #vertical line 
    if real_close(seg.x1,seg.x2)
      if real_close(@x,seg.x1)
        seg
      else 
        NoPoints.new()
      end
    elsif inbetween(@x,seg.x1,seg.x2)
      line.intersectVerticalLine self 
    else 
      NoPoints.new()
    end
  end
end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and 
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def preprocess_prog
    if real_close_point(@x1,@y1,@x2,@y2)
      Point.new(@x1,@y1)
    elsif real_close(@x1,@x2) 
      if @y2 < @y1
        LineSegment.new(@x2,@y2,@x1,@y1)
      else 
        self
      end
    elsif @x2 < @x1
      LineSegment.new(@x2,@y2,@x1,@y1)
    else @x1 < @x2
      self
    end
  end

  def shift(dx,dy)
    LineSegment.new(@x1 + dx, @y1 + dy, @x2 + dx, @y2 + dy)
  end

  def intersect other
    other.intersectWithSegmentAsLineResult self 
  end
  def intersectPoint p
    p.intersectWithSegmentAsLineResult self
  end
  def intersectLine line
    line.intersectWithSegmentAsLineResult self
  end
  def intersectVerticalLine vline
    vline.intersectWithSegmentAsLineResult self
  end

  #Case of 2 line segments 
  def intersectWithSegmentAsLineResult seg
    if real_close_point(@x1,@y1,seg.x1,seg.y1) && real_close_point(@x2,@y2,seg.x2,seg.y2)
      self
    else 
      #Seg is a vertical line 
      if real_close(seg.x1,seg.x2)
        #seg b always has the greater y1
        seg_a = LineSegment.new(seg.x1,seg.y1,seg.x2,seg.y2)
        seg_b = LineSegment.new(@x1,@y1,@x2,@y2)   
        if seg.y1 > @y1 
          seg_a = LineSegment.new(@x1,@y1,@x2,@y2)   
          seg_b = LineSegment.new(seg.x1,seg.y1,seg.x2,seg.y2) 
        end
        if real_close(seg_a.y2,seg_b.y1)
          #just touching
          Point.new(seg_a.x2,seg_a.y2)
        elsif seg_a.y2 < seg_b.y1
          #disjoint
          NoPoints.new()
        elsif seg_a.y2 > seg_b.y2
          # b inside of a
          if real_close(seg_b.y1,seg_b.y2)
            #vertical and horizontal lines
            Point.new(seg_a.x1,seg_b.y1)
          else 
            seg_b
          end
        elsif real_close(seg_a.x1,seg_a.x2) && real_close(seg_b.x1,seg_b.x2)
          #two verticals
          if real_close(seg_a.x1,seg_b.x1)
            LineSegment.new(seg_b.x1,seg_b.y1,seg_a.x2,seg_a.y2)
          else 
            NoPoints.new()
          end
        else 
          #overlapping
          LineSegment.new(seg_b.x1,seg_b.y1,seg_a.x2,seg_a.y2)
        end
      #Not vertical line 
      else 
        seg_a = LineSegment.new(seg.x1,seg.y1,seg.x2,seg.y2) 
        seg_b = LineSegment.new(@x1,@y1,@x2,@y2)  
        if seg.x1 > @x1 
          seg_a = LineSegment.new(@x1,@y1,@x2,@y2)  
          seg_b = LineSegment.new(seg.x1,seg.y1,seg.x2,seg.y2)
        end
        if real_close(seg_a.x2,seg_b.x1)
          #just touching 
          Point.new(seg_a.x2,seg_a.y2)
        elsif seg_a.x2 < seg_b.x1
          #disjoint 
          NoPoints.new()
        elsif seg_a.x2 > seg_b.x2
          #b inside a
          seg_b
        elsif real_close(seg_a.y1,seg_a.y2) && real_close(seg_b.y1,seg_b.y2)
          #two horizontals
          if real_close(seg_a.y1,seg_b.y1)
            if seg_b.x1 > seg_a.x1
                LineSegment.new(seg_b.x1,seg_b.y1,seg_a.x2,seg_b.y2)
            else
                LineSegment.new(seg_a.x1,seg_b.y1,seg_a.x2,seg_b.y2)
            end
          else 
            NoPoints.new()
          end
        else
          #overlapping 
          LineSegment.new(seg_b.x1,seg_b.y1,seg_a.x2,seg_a.y2)
        end
      end
    end
  end
end

# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end
  def preprocess_prog
    Intersect.new(@e1.preprocess_prog,@e2.preprocess_prog)
  end
  def eval_prog env
    @e1.eval_prog(env).intersect(@e2.eval_prog(env))
  end
end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end
  def preprocess_prog
    Let.new(@s,@e1.preprocess_prog,@e2.preprocess_prog)
  end
  def eval_prog env
    new_env = [[@s,@e1.eval_prog(env)]] + env
    @e2.eval_prog(new_env)
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end
  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end
  def preprocess_prog
    Shift.new(@dx,@dy,@e.preprocess_prog)
  end
  def eval_prog env
    @e.eval_prog(env).shift(@dx,@dy)
  end
end


### Testing

#Points
# e1 = Point.new(3,4)
# s1 = Shift.new(1,4,e1)
# result = s1.eval_prog []
# puts result.inspect

# #Line segments
# ls1 = LineSegment.new(1,3,2,5)
# result = ls1.preprocess_prog
# puts result.inspect

# #Point - Point
# e2 = Point.new(3,5)
# result = e1.intersect(e2)
# puts result.inspect
# result = e1.intersect(e1)
# puts result.inspect

#Line seg - Point 
# d = LineSegment.new(1,2,-3,-4).preprocess_prog
# d2 = d.intersect(LineSegment.new(2,3,4,5))
# puts d2.inspect

#Lines
# l1 = LineSegment.new(2,0,2,1)
# l2 = LineSegment.new(0,2,4,2)
# i = Intersect.new(l1,l2).preprocess_prog.eval_prog([])
# puts i.inspect