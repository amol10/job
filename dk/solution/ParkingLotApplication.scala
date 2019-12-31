import scala.util.control.Breaks._

object debugger {
    var enabled: Boolean = false

    def debug(msg: String) {
        if (!enabled)
            return
        println("DEBUG: " + msg)
    }

    def array(arr:Array[String]) {
        if (!enabled)
            return
        if (arr == null) {
            return
        }
        for (i <- arr) {
            println("DEBUG: " + i)
        }
    }
}

class ParkingLot {
    var size: Int = 0
    var parked: Array[String] = new Array[String](0)

    def create(size: Int){
        this.size = size
        this.parked = new Array[String](size)
    }

    def park(reg: String): (Boolean, Int) = {
        var foundSlot: Boolean = false
        var slotNo: Int = -1

        breakable {
            for (i <- 0 to size - 1) {
                if (parked(i) == null) {
                    parked(i) = reg
                    foundSlot = true
                    slotNo = i + 1
                    break
                }
            }
        }

        return (foundSlot, slotNo)
    }

    def leave(reg: String, hours: Int): (Boolean, Int, Int) = {
        var vehicleIdx: Int = parked.indexOf(reg, 0)

        if (vehicleIdx < 0) {
            return (false, -1, -1)
        }
        parked(vehicleIdx) = null

        return (true, vehicleIdx + 1, calc_charge(hours))
    }

    def calc_charge(hours: Int): Int = {
        var rateInit2: Int = 10
        var rateAdd1: Int = 10
        var charge: Int = 0

        for (h <- 1 to hours) {
            if (h == 1)
                charge += rateInit2
            if (h > 2)
                charge += rateAdd1
        }

        return charge
    }

    def status(): Array[(Int, String)] = {
        var slots: Array[(Int, String)] = new Array[(Int, String)](0)
        for (i <- 0 to size - 1) {
            if (parked(i) != null)
                slots = slots :+ (i + 1, parked(i))
        }

        return slots
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
        var loop: Boolean = true

        breakable {
            while(loop) {
                var command: String = ""
                var params: Array[String] = null
                var line = Console.readLine
                if (line == null) {
                    break
                }
                var parts: Array[String] = line.split(" ")

                command = parts(0)
                if (parts.length > 1) {
                    params = parts.slice(1, parts.length)
                }
                debugger.debug("command, params: " + command)
                debugger.array(params)
                if (command == "exit") {
                    loop = false
                } else {
                    var msg: String = this.dispatch(command, params)
                    println(msg)
                }
            }
        }
    }

    def dispatch(command: String, params:Array[String]): String = {
        return this.connector.dispatchCommand(command, params)
    }

}

object Messages {
    var parkingLotCreated: String = "Created parking lot with %d slots"
    var slotAllocated: String = "Allocated slot number: %d"
    var slotFree: String = "Registration number %s with Slot Number %d is free with Charge %d"
    var vehicleNotFound = "Registration number %s not found"
    var parkingLotFull = "Sorry, parking lot is full"
    var statusHeader = "Slot No.\tRegistration No."
    var statusLine = "%d\t\t%s"
}


class Connector {
    var cmdInterface: CommandInterface = new CommandInterface()
    var parkingLot: ParkingLot = new ParkingLot()

    def connect() {
        cmdInterface.setConnector(this)
        cmdInterface.start()
    }

    def dispatchCommand(command: String, params: Array[String]): String = {
        var returnString: String = ""

        command match {
            case "status" => {
                var status: Array[(Int, String)] = this.parkingLot.status()

                returnString = Messages.statusHeader + "\n"
                for ((no, reg) <- status) {
                    returnString += Messages.statusLine.format(no, reg) + "\n"
                }
                returnString = returnString.trim()
            }

            case "park" => {
                var (success: Boolean, slotNo: Int) = this.parkingLot.park(params(0))
                if (!success)
                    returnString = Messages.parkingLotFull
                else
                    returnString = Messages.slotAllocated.format(slotNo)
            }

            case "leave" => {
                var (success: Boolean, slotNo: Int, charge: Int) = this.parkingLot.leave(params(0), params(1).toInt)

                if (success)
                    returnString = Messages.slotFree.format(params(0), slotNo, charge)
                else
                    returnString = Messages.vehicleNotFound.format(params(0))
            }

            case "create_parking_lot" | "c" => {
                var size: Int = params(0).toInt
                this.parkingLot.create(size)
                returnString = Messages.parkingLotCreated.format(size)
            }
        }

        return returnString        
    }
}

class Application {
    var connector: Connector = new Connector()
    def start() {
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
