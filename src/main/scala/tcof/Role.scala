package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

/** Represents a role in an ensemble. Implements methods to build membership over components contained in a role. */
class Role[+ComponentType <: Component](val name: String, private[tcof] val parent: WithRoles, private[tcof] val allMembers: RoleMembers[ComponentType]) extends WithMembers[ComponentType] with Initializable {

  def cloneEquiv = new RoleMembersEquiv(this)

  def cloneCond = new RoleMembersCond(this)

  def ++[OtherType >: ComponentType <: Component](other: Role[OtherType]): Role[OtherType] = {
    require(parent == other.parent)
    parent.role(cloneEquiv ++ other.cloneEquiv)
  }

  override def toString: String =
    s"""Role "$name":\n"""

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)
    allMembers._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        allMembers.mapChildToParent(this)
      case _ =>
    }
  }
}
