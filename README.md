# Honeypot (under development)

## Game Description
Disaster! The evil robot overlords have built an army of attack drones to 
terrorize the Earth. Humanity's only hope is an ingenious new automated battle 
tank designed to combat the new threat. After several months of frantic work, 
the tank's construction has finally been completed. Now all that remains is to 
program it to take on the robot army. That is where you come in.

The tank and drones move on a two-dimensional grid. Movement on the grid can 
only be done horizontally and vertically, and only one square at a time. Your 
job is to program the tank to seek out and destroy as many drones and other 
targets as possible before its fuel runs out. The exact capabilities of the 
attack drones are currently not known, but reverse-engineering has shown that 
they do not appear to be equipped with any ranged weapons and will have to get 
close to attack.

The tank is equipped with state-of-the-art technology. Its caterpillar tracks 
allow backward and forward movement, and 90-degree turns to the left and right. 
It has a forward-facing cannon, ready to fire at any enemy caught in the sights 
of the tank's highly advanced target identification system. In addition, the 
tank is equipped with a high-precision lidar system, to measure the distance to 
objects in its surroundings.

The tank is also equipped with a shield generator, and a low-consumption fuel 
cell. Moving around and firing the cannon consumes fuel, and being attacked by 
drones or hitting walls causes the shield generator to drain even more fuel. The 
lidar and target identification systems are solar powered and do not consume any 
fuel.

### Task Overview
This game is about programming an AI for a tank to survive in a unfamiliar and
hostile environment. You provide the AI by programming a function with the
following type signature

```haskell
playerStep :: Step Event
```

The `Step Monad` allows you to retrieve information about the surrounding and 
current state of the game, such as remaining fuel, and number of empty cells 
between the tank and obstacles  

```haskell
playerStep :: Step Event 
playerStep = do
  fuel <- currentFuel
  cell <- identifyTarget
  nr <- lidarFront
  // do some calculations on the information
  let event = calculateEvent fuel cell nr
  return event
```

### Step Monad Api
Inside the `Step Monad` you have access to the following functions:

Returns how much fuel the tank has left.
```haskell
currentFuel :: Step Fuel
```

Returns the character of the cell infront of the tank.
```haskell
identifyTarget :: Step Cell

data Cell = Empty
          | Wall
          | Block
          | Enemy
```

Measures the distance in squares to the nearest object infront/behind/beside the tank. 
Objects on a square directly adjacent to the tank will have a distance of 1. 
Lidar systems do not consume any fuel.
```haskell
lidarFront, lidarBack, lidarRight, lidarLeft :: Step Int
```

Your desicion is reflected by the type of `Event` you return from the `Step Monad`. All 
event are associated with a fuel cost.
```haskell
data Event = TurnLeft     -- 1 fuel
           | TurnRight    -- 1 fuel
           | MoveForward  -- 1 fuel
           | MoveBackward -- 1 fuel
           | Noop         -- 1 fuel
           | Shoot        -- 5 fuel
```
