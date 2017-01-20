package chetak

/**
  * Created by tushark on 16/1/17.
  */
package object json {

  type JField = (String, JValue)

  sealed abstract class JValue
  case object JNothing extends JValue // 'zero' for JValue
  case object JNull extends JValue
  case class JString(s: String) extends JValue
  case class JDouble(num: Double) extends JValue
  case class JDecimal(num: BigDecimal) extends JValue
  case class JInt(num: BigInt) extends JValue
  case class JLong(num: Long) extends JValue
  case class JBool(value: Boolean) extends JValue
  case class JObject(obj: List[JField]) extends JValue
  case class JArray(arr: List[JValue]) extends JValue


}
