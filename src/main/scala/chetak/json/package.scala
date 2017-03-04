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

  case class JsonPath(list: List[String]) {
    def /(next: String) : JsonPath = JsonPath(next :: list)
  }

  val JSONROOT = JsonPath(List[String]())

  def jsonQuery(json: JValue, path: JsonPath): Option[JValue] = _jsonQuery(json, JsonPath(path.list.reverse))

  def _jsonQuery(json: JValue, path: JsonPath): Option[JValue] = {
    //println(s"querying for ${path.list} on ${json}")
    path.list match {
      case head :: tail =>
        json match {
          case jobj: JObject =>
            //println(s"Finding $head")
            jobj.obj.find( _._1 == head) match {
              case Some(f) =>
                _jsonQuery(f._2, JsonPath(tail))
              case None =>
                None
            }
          case _ => throw new RuntimeException(s"Reached leaf level. No object at ${path} path")
        }
      case Nil =>
        Some(json)
    }
  }


}
