package rcrs.scenario

/** Representation of the component's state, transferred between component and ensemble
  * and used in computations on the initiator of the ensemble. */
object MirrorState extends Enumeration {
  type MirrorState = Value
  val ProtectingMirror, RefillingMirror, IdleMirror = Value
}
