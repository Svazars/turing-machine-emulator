package nsu.turing

import scala.collection.mutable

object CmdParser {

  def parseCmd(cmd: Array[String]): (Set[ProgramOption], Map[ProgramArg[_], String]) = {
    val args = mutable.Map[ProgramArg[_], String]()
    val ops = mutable.Set[ProgramOption]()

    var i = 0
    while (i < cmd.length) {
      val key: String = cmd(i)

      checkArg(key) match {
        case Some(arg) =>
          if (i < cmd.length - 1) {
            val value = cmd(i + 1)
            args.put(arg, value)
            i = i + 2
          } else {
            throw new UnsupportedOperationException(s"Expected value for $key argument.")
          }

        case None => checkOption(key) match {
          case Some(o) =>
            ops += o
            i = i + 1
          case None =>
            throw new UnsupportedOperationException(s"unexpected parameter $key")
        }
      }
    }

    (ops.toSet, args.toMap)
  }

  def processInput(raw: String, tapeSeparator: String, cellSeparator: Option[String]): Array[List[base.Symbol]] =
    raw.split(tapeSeparator) map {
      arr => (cellSeparator match {
        case Some(s) => arr.split(s).toList
        case None => arr.toList map { c => c.toString }
      }) map base.Symbol.apply
    }

  val DEFAULT_MAX_TRANSITIONS: String = BigInt(10 * 1000).toString()
  val DEFAULT_TAPE_SEPARATOR = "|"
  val DEFAULT_START_STATE = "START"
  val DEFAULT_TERMINATION_STATE = "STOP"

  private def checkArg(key: String) = Arguments.list.find(_.name.equals(key))
  private def checkOption(key: String) = Options.list.find { _.name.equals(key) }
}
