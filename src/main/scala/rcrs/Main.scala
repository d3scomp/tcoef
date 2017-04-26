package rcrs

import rcrs.nosolver.{SimpleCentralAgent, SimpleFireBrigadeAgent}
import rescuecore2.Constants
import rescuecore2.components.TCPComponentLauncher
import rescuecore2.config.Config
import rescuecore2.log.Logger
import rescuecore2.misc.CommandLineOptions
import rescuecore2.registry.Registry
import rescuecore2.standard.entities.{StandardEntityFactory, StandardPropertyFactory}
import rescuecore2.standard.messages.StandardMessageFactory

object Main {
  def main(args: Array[String]): Unit = {
    Registry.SYSTEM_REGISTRY.registerEntityFactory(StandardEntityFactory.INSTANCE)
    Registry.SYSTEM_REGISTRY.registerMessageFactory(StandardMessageFactory.INSTANCE)
    Registry.SYSTEM_REGISTRY.registerPropertyFactory(StandardPropertyFactory.INSTANCE)

    val config = new Config
    val restArgs = CommandLineOptions.processArgs(args, config)

    val port = config.getIntValue(Constants.KERNEL_PORT_NUMBER_KEY, Constants.DEFAULT_KERNEL_PORT_NUMBER)
    var host = config.getValue(Constants.KERNEL_HOST_NAME_KEY, Constants.DEFAULT_KERNEL_HOST_NAME)

    val launcher = new TCPComponentLauncher(host, port, config)

    Logger.info("Connecting fire brigades ... ")
    for (_ <- 1 to 5) {
//      launcher.connect(new FireBrigadeAgent().rcrsAgent)
      launcher.connect(new SimpleFireBrigadeAgent().rcrsAgent)
    }

    Logger.info("Connecting central ... ")
//    launcher.connect(new CentralAgent().rcrsAgent)
    launcher.connect(new SimpleCentralAgent().rcrsAgent)

    Logger.info("success")
  }

}


