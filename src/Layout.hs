{-# LANGUAGE RecordWildCards #-}
module Layout
    ( forces
    )
where

import Control.Concurrent
import Linear.Affine

import qualified RigidBody.Physics as Physics

import Data.Time.Clock
import Linear


data State = State
    { particles :: [Particle]
    }

data Particle = Particle
    { pos :: V2 Float
    , vel :: V2 Float
    }

type DTime = NominalDiffTime

type Input = ()

update :: Input -> State -> State
update i s = State
    { particles = np
    }
  where
    np = either (const $ particles s) (map stateParticle . Physics.particles) next
    next = Physics.bigStep (Physics.Forces [0.1]) 0.01 world
    world = Physics.World
        { particles = map (mkParticle $ particles s) (particles s)
        }

-- TODO use @particles@
mkParticle :: [Particle] -> Particle -> Physics.Particle Float
mkParticle particles Particle{..} = Physics.Particle
    { Physics.mass = 1
    , pos = P pos
    , vel = vel
    , Physics.forces = []
    }

stateParticle :: Physics.Particle Float -> Particle
stateParticle Physics.Particle{..} = Particle
    { pos = pos .-. 0
    , vel = vel
    }

forces :: MVar Input -> MVar State -> IO ()
forces input state = do
    i <- readMVar input
    modifyMVar_ state (pure . update i)
