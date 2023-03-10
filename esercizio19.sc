import java.io.FileWriter

class Conto(var saldoIn: Double) {

  protected var saldo = saldoIn

  def deposito(importo: Double): Unit = {
    saldo += importo
  }

  def prelievo(importo: Double): Unit = {
    saldo -= importo
  }

  def getSaldo() = {this.saldo}
}

trait Logger {
  def log(message: String): Unit
}

trait ConsoleLogger extends Logger {
  def log(message: String): Unit = {
    println(message)
  }
}

trait ShortLogger extends ConsoleLogger {
  override def log(message: String): Unit = {
    super.log(if(message.length <= 15) message else s"$message.substring(0,12)")
  }
}

trait TimeLogger extends Logger {
  def log(message: String): Unit = {
    println(s"${java.time.Instant.now()}: $message")
  }
}

trait FileLogger extends Logger {
  def fileName : String
  def log(message: String): Unit = {
    val wrt = new FileWriter(fileName, true)
    wrt.append(message+"\n")
    wrt.flush()
    wrt.close()
  }
}

trait CaesarLogger extends Logger {
  def shift:Int = 5
  def log(message: String): Unit = {
    val shiftedMessage = message.map(c => (c + shift).toChar)
    println(shiftedMessage)
  }
}

abstract class ContoConLog(sIn: Double) extends Conto(sIn) with Logger {
  override def prelievo(x: Double) = {
    if (x > super.getSaldo()) {
      log("Saldo insufficiente")
    } else {
      log(s"Prelievo di ${x}")
    }
  }

  override def deposito(x: Double): Unit = {
    log(s"Deposito di ${x}")
    super.deposito(x)         //metodo classe Conto
  }

  override def getSaldo() = {
    log(s"Richiesta saldo disponibile su conto")
    super.getSaldo()       //metodo classe Conto
  }
}

var c2 = new ContoConLog(20d) with ConsoleLogger

def operazioniConto1(c : Conto) : Unit = {
  c.deposito(100d)
  c.prelievo(20d)
  c.getSaldo()
  c.deposito(40d)
  c.prelievo(60d)
  c.deposito(40d)
}

operazioniConto1(new ContoConLog(10d) with ConsoleLogger)
operazioniConto1(new ContoConLog(20d) with FileLogger{
  override def fileName: String = "path"
})
operazioniConto1(new ContoConLog(12d) with CaesarLogger)

operazioniConto1(new ContoConLog(18d) with ShortLogger)

operazioniConto1(new ContoConLog(20d) with TimeLogger)