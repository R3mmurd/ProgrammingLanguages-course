require_relative './section-8-provided'

class Character
  def initialize hp
    @hp = hp
  end

  def resolve_encounter enc
    if !is_dead?
      play_out_encounter enc
    end
  end

  def is_dead?
    @hp <= 0
  end

  private

  def play_out_encounter enc
    ## YOUR CODE HERE
    enc.play_out self
  end
end

class Knight < Character
  def initialize(hp, ap)
    super hp
    @ap = ap
  end

  def to_s
    "HP: " + @hp.to_s + " AP: " + @ap.to_s
  end

  ## YOUR CODE HERE

  def play_out_floor_trap floor_trap
    damage floor_trap.dam
  end

  def play_out_monster monster
    damage monster.dam
  end

  def play_out_potion potion
    @hp += potion.hp
  end

  def play_out_armor armor
    @ap += armor.ap
  end

  private
  
  def damage dam
    if @ap == 0
      @hp -= dam
    elsif dam > @ap
      new_dam = dam - @ap
      @ap = 0
      damage new_dam
    else
      @ap -= dam
    end
  end
      
end

class Wizard < Character
  def initialize(hp, mp)
    super hp
    @mp = mp
  end

  def to_s
    "HP: " + @hp.to_s + " MP: " + @mp.to_s
  end

  ## YOUR CODE HERE
  def play_out_floor_trap floor_trap
    if @mp > 0
      @mp -= 1
    else
      @hp -= floor_trap.dam
    end
  end

  def play_out_monster monster
    @mp -= monster.hp
  end

  def play_out_potion potion
    @hp += potion.hp
    @mp += potion.mp
  end

  def play_out_armor armor

  end
end

class FloorTrap < Encounter
  attr_reader :dam

  def initialize dam
    @dam = dam
  end

  def to_s
    "A deadly floor trap dealing " + @dam.to_s + " point(s) of damage lies ahead!"
  end

  ## YOUR CODE HERE
  def play_out character
    character.play_out_floor_trap self
  end
end

class Monster < Encounter
  attr_reader :dam, :hp

  def initialize(dam, hp)
    @dam = dam
    @hp = hp
  end

  def to_s
    "A horrible monster lurks in the shadows ahead. It can attack for " +
        @dam.to_s + " point(s) of damage and has " +
        @hp.to_s + " hitpoint(s)."
  end

  ## YOUR CODE HERE
  def play_out character
    character.play_out_monster self
  end
end

class Potion < Encounter
  attr_reader :hp, :mp

  def initialize(hp, mp)
    @hp = hp
    @mp = mp
  end

  def to_s
    "There is a potion here that can restore " + @hp.to_s +
        " hitpoint(s) and " + @mp.to_s + " mana point(s)."
  end

  ## YOUR CODE HERE
  def play_out character
    character.play_out_potion self
  end
end

class Armor < Encounter
  attr_reader :ap

  def initialize ap
    @ap = ap
  end

  def to_s
    "A shiny piece of armor, rated for " + @ap.to_s +
        " AP, is gathering dust in an alcove!"
  end

  ## YOUR CODE HERE
  def play_out character
    character.play_out_armor self
  end
end

if __FILE__ == $0
  Adventure.new(Stdout.new, Knight.new(15, 3),
    [Monster.new(1, 1),
    FloorTrap.new(3),
    Monster.new(5, 3),
    Potion.new(5, 5),
    Monster.new(1, 15),
    Armor.new(10),
    FloorTrap.new(5),
    Monster.new(10, 10)]).play_out
end
