package rcrs.traits

import rescuecore2.worldmodel.EntityID
import tcof.WithID

trait WithEntityID extends WithID {
  type IDType = EntityID
}
