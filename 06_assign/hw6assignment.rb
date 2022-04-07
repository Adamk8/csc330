# Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in,
# so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here:
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                  [[0, 0], [0, -1], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]),
                  [[[0,0],[0,-1],[0,-2],[0,1],[0,2]],# 2 rotations for long line
                  [[0,0],[-1,0],[-2,0],[1,0],[2,0]]],
                  rotations([[0,0],[0,1],[1,0]]), # all rotation for mini L
                  rotations([[0,0],[0,1],[1,0],[1,1],[0,2]]), # all rotations for square with extra pixel
                ] # Z
  # Your Enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
  def self.cheat_piece (board)
    MyPiece.new([[[0,0]]], board)
  end
end

class MyBoard < Board
  # Your Enhancements here:
  def initialize (game)
    super
    @current_block = MyPiece.next_piece(self)
  end
  # gets the next piece
  def next_piece
    if @cheat_queued
      @current_block = MyPiece.cheat_piece(self)
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
    @cheat_queued = false
  end

  # 180 rotate
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  #cheat function
  def cheat
    if score >= 100 && !@cheat_queued 
      @score = @score - 100
      @cheat_queued = true
    end
  end

  # Update the mapping because original had 4 indexes hardcoded, 
  #the new pieces need 1,3,and 5, so use a map instead
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    locations.map.with_index {|pixel, index| 
      current = pixel
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # Your Enhancements here:
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  def key_bindings  
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end
end
