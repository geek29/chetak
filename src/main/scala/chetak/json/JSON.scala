package chetak.json

import scala.collection.mutable.ArrayBuffer

/**
  * Created by tushark on 18/1/17.
  */
object JSON {

  val STRING_TOKEN = 1
  val NON_STRING_TOKEN = 2
  val OBJECT_TOKEN = 3
  val ARRAY_TOKEN = 4
  val OBJECT_END_TOKEN = 5
  val ARRAY_END_TOKEN = 6
  val PROP_DIVIDER_TOKEN = 7

  def readObject(implicit pc : JSONParseContext) : JObject = {
    val list = new ArrayBuffer[JField]
    pc.skipSpaces
    pc.readObjectStart
    pc.skipSpaces
    while(!pc.isObjectEnd) {
      pc.skipSpaces
      val x = readProperty
      list.append(x)
      pc.skipSpaces
      pc.nextToken() match {
        case PROP_DIVIDER_TOKEN => pc.readPropertySeparator
        case OBJECT_END_TOKEN =>
        case _ => throw  new RuntimeException("Invalid token expected PROP_DIVIDER_TOKEN or OBJECT_END_TOKEN")
      }
    }
    pc.readObjectEnd
    JObject(list.toList)
  }

  def readArray(implicit pc : JSONParseContext) : JArray = {
    val list = new ArrayBuffer[JValue]
    pc.skipSpaces
    pc.readArrayStart
    while(!pc.isArrayEnd) {
      pc.skipSpaces
      readValue match {
        case JNothing => //Array Ended
        case x =>
          list.append(x)
          pc.skipSpaces
          pc.nextToken() match {
            case PROP_DIVIDER_TOKEN => pc.readPropertySeparator
            case ARRAY_END_TOKEN =>
            case _ => throw  new RuntimeException("Invalid token expected PROP_DIVIDER_TOKEN or ARRAY_END_TOKEN")
          }
      }
    }
    pc.readArrayEnd
    JArray(list.toList)
  }

  def debug(f : () => String) = {
    //println(f())
  }

  def readProperty(implicit pc : JSONParseContext) : JField = {
    debug { () => s"Read property start at ${pc.index}" }
    pc.skipSpaces()
    val x1 = pc.readString()
    debug { () => (s"Read property key at ${pc.index} <$x1>") }
    pc.skipSpaces()
    pc.readPropertyDivider
    pc.skipSpaces
    val x2 = readValue
    debug { () => (s"Read property value at ${pc.index} <$x2>") }
    (x1, x2)
  }

  def readValue(implicit pc: JSONParseContext) : JValue = {
    pc.nextToken() match {
      case STRING_TOKEN => JString(pc.readString())
      case NON_STRING_TOKEN => getJValue(pc.readNonString().trim)
      case OBJECT_TOKEN => readObject(pc)
      case ARRAY_TOKEN => readArray(pc)
      case OBJECT_END_TOKEN => JNothing
      case ARRAY_END_TOKEN => JNothing
    }
  }

  def getJValue(str: String) = {
    str match {
      case "true" => JBool(str.toBoolean)
      case "false" => JBool(str.toBoolean)
      case _ =>
        if(str.contains("."))
          JDouble(str.toDouble)
        else JInt(str.toInt)
    }
  }

  def json(json: JObject): String = {
    var first = true
    val b = new StringBuilder()
    b append "{"
    for (x <- json.obj) {
      if (first) {
        b.append("\"").append(x._1).append("\":").append(jValueJson(x._2))
        first = false
      }
      else {
        b.append(",")
        b.append("\"").append(x._1).append("\":").append(jValueJson(x._2))
      }
    }
    b.append("}")
    b.toString()
  }

  def jValueJson(jvalue: JValue): String = {
    val sb = new StringBuilder()
    jvalue match {
      case k:JString => sb.append("\"").append(k.s).append("\"")
      case k:JBool => sb.append(k.value)
      case k:JDouble => sb.append(k.num)
      case k:JInt => sb.append(k.num)
      case k:JObject => sb.append(json(k))
      case k:JArray => sb.append(json(k))
    }
    sb.toString()
  }

  def json(json: JArray): String = {
    var first = true
    val b = new StringBuilder()
    b.append("[")
    for (x <- json.arr) {
      if (first) {
        b.append(jValueJson(x))
        first = false
      }
      else {
        b.append(",")
        b.append(jValueJson(x))
      }
    }
    b.append("]")
    b.toString()
  }

  def parseObject(str: String) = {
    try {
      readObject(new JSONParseContext(str))
    } catch {
      case e: Exception => throw new RuntimeException("Error parsing JSON", e)
    }
  }

  def parseArray(str: String) = {
    try {
      readArray(new JSONParseContext(str))
    } catch {
      case e: Exception => throw new RuntimeException("Error parsing JSON", e)
    }
  }

}

class JSONParseContext(str: String) {

  val array = str.toCharArray

  var index = 0

  def next() = {
    index = index + 1
    array(index)
  }

  def skip() = {
    index = index + 1
  }

  def checkIndex = (index <= array.length -1)

  def skipSpace(c: Char): Boolean = array(index) == c

  def nextToken() = {
    if(array(index) == '[') {
      JSON.debug { () => s"Reading array token at $index" }
      JSON.ARRAY_TOKEN
    } else if(array(index) == '{'){
      JSON.debug { () => (s"Reading object start token at $index") }
      JSON.OBJECT_TOKEN
    } else if(array(index) == ']') {
      JSON.debug { () => (s"Reading array  end token at $index") }
      JSON.ARRAY_END_TOKEN
    } else if(array(index) == '}'){
      JSON.debug { () => (s"Reading object end token at $index") }
      JSON.OBJECT_END_TOKEN
    } else if(array(index) == '"'){
      JSON.debug { () => (s"Reading string token at $index") }
      JSON.STRING_TOKEN
    } else if(array(index) == ','){
      JSON.debug { () => (s"Reading prop divider token at $index") }
      JSON.PROP_DIVIDER_TOKEN
    } else {
      JSON.debug { () => (s">>>Reading non-string token at $index") }
      JSON.NON_STRING_TOKEN
    }

  }

  def readArrayStart() = {
    if(array(index) == '[') {
      index = index + 1
      JSON.debug { () => (s"Read array start at $index") }
    } else
      throw new RuntimeException("Invalid start of array")
  }


  def readArrayEnd() = {
    if(array(index) == ']') {
      index = index + 1
      JSON.debug { () => (s"Read array end at $index") }
    } else
      throw new RuntimeException("Invalid end of array")
  }

  def isArrayEnd() = array(index) == ']'


  def readObjectStart() = {
    if(array(index) == '{') {
      index = index + 1
      JSON.debug { () => (s"Read object start at $index") }
    } else
      throw new RuntimeException("Invalid start of object")
  }

  def isObjectEnd() = array(index) == '}'

  def readObjectEnd() = {
    if(array(index) == '}') {
      index = index + 1
      JSON.debug { () => (s"Read object end at $index") }
    } else
      throw new RuntimeException("Invalid end of object")
  }

  def skipWhiteSpace() : Boolean = {
    val checkWhiteSpace = skipSpace(' ')
    val checkNewLine = skipSpace('\n')
    val checkCR = skipSpace('\r')
    val checkTab = skipSpace('\t')
    checkWhiteSpace || checkNewLine || checkTab || checkCR
  }


  def skipSpaces() = {
    val oldIndex = index
    val sb = new StringBuilder
    var move = checkIndex && skipWhiteSpace
    while(move) {
      sb.append(array(index))
      index = index + 1
      move =  checkIndex && skipWhiteSpace
    }
    JSON.debug { () => (s"skipSpaces at $oldIndex <$sb>") }
    index
  }

  def readPropertyDivider() = {
    if(array(index) == ':') {
      index = index + 1
      JSON.debug { () => (s"Read readPropertyDivider at $index") }
    } else throw new RuntimeException("Property divider missing..")
  }

  def readPropertySeparator() = {
    if(array(index) == ',') {
      index = index + 1
      JSON.debug { () => (s"Read readPropertySeparator at $index") }
    } else throw new RuntimeException("Property separator missing..")
  }

  def readString(): String = {
    if(array(index) == '"') {
      val sb = new StringBuilder
      var newIndex = index + 1
      var stop = false
      var escape = false
      while(!stop) {
        val ch = array(newIndex)
        //Reading a escape modifier
        if (ch == '\\') {
          escape = true
          newIndex = newIndex + 1
        }
        else {
          //double quote encoutered
          if (ch == '"') {
            //previous char is not \\ thus this is end of string
            if (escape) {
              sb.append(ch)
              newIndex = newIndex + 1
            }
            else {
              stop = true
              newIndex = newIndex + 1
            }
          } else {
            sb.append(ch)
            newIndex = newIndex + 1
          }
          escape = false
        }
      }
      index = newIndex
      JSON.debug { () => (s"Read String at $index <$sb>") }
      sb.toString()
    } else throw new RuntimeException("Invalid start for string")
  }

  def readNonString(): String = {
    val sb = new StringBuilder
    var newIndex = index
    var stop = false
    while(!stop) {
      val ch = array(newIndex)
      if(ch == ',' || ch ==']' || ch == '}') {
        stop = true
      } else {
        sb.append(ch)
        newIndex = newIndex + 1
      }
    }
    index = newIndex
    JSON.debug { () => (s"Read String at $index <$sb>") }
    sb.toString().trim
  }

  def peek() = {
    val sb = new StringBuilder
    for(i <- index until array.length)
      sb.append(array(i))
    sb.toString()
  }

}