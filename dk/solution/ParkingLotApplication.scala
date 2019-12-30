
object debugger {
    var enabled: Boolean = true

    def debug(msg: String) {
        println(msg)
    }
}

class ParkingLot {
    var size: Int = 0
    var parked: Array[String] = new Array[String](0)

    def create(size: Int){
        this.size = size
        this.parked = new Array[String](size)
    }

    def park(reg: String) {
        parked :+= reg
    }

    def leave(reg: String) {

    }

    def status() {
        println("status")
    }
}

class CommandInterface {
    var connector: Connector = null

    def CommandInterface() {
    }

    def setConnector(connector: Connector) {
        this.connector = connector
    }

    def start() {
        var command: String = ""
        var param: String = ""
        var loop: Boolean = true
        while(loop) {
            var line = Console.readLine
            var parts: Array[String] = line.split(" ")

            command = parts(0)
            if (parts.length > 1) {
                param = parts(1)
            }
            debugger.debug("command, param: " + command + param)
            if (command == "exit") {
                loop = false
            } else {
                this.dispatch(command, param)
                command = ""
                param = ""
            }
        }
    }

    def dispatch(command: String, param: String) {
        this.connector.dispatchCommand(command, param)
    }

}


class Connector {
    var cmdInterface: CommandInterface = new CommandInterface()
    var parkingLot: ParkingLot = new ParkingLot()

    def connect() {
        cmdInterface.setConnector(this)
        cmdInterface.start()
    }
    //public
    def dispatchCommand(command: String, param: String) {
        command match {
            case "status" => this.parkingLot.status()
            case "park" => this.parkingLot.park(param)
            case "leave" => this.parkingLot.leave(param)
        }
        
    }
}

class Application {
    var connector: Connector = new Connector()
    def start() {
        debugger.debug("App ctor")
        this.connector.connect()
    }
}


object ParkingLotApplication {
    def main(args: Array[String]) {
        debugger.debug("start")
        var app: Application = new Application()
        app.start()
    }
}

