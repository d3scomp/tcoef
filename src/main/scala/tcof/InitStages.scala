package tcof

object InitStages extends Enumeration {
  type InitStages = Value

  val EraseAllStates, CreateCustomStates, ExtraDeclarations, ConfigPropagation, VarsCreation, RulesCreation = Value
}
