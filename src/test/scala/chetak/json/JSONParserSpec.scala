package chetak.json

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by tushark on 16/1/17.
  */
class JSONParserSpec extends FlatSpec with Matchers {

  val validJSON =
    """{
      | "numbers" : [1, 2, 3, 4],
      | "stringKey" : "JString\\n \\n Value",
      |  "intKey" : 123456,
      |  "decimalKey: 1234.5678,
      | }""".stripMargin

  "JSONParser" should "be able to parse simple object" in {
    val string =
      """ { "k1" : "stringval",
        |"k2" : 1234,
        |"k3" : true,
        |"k4" : 1234.5678}""".stripMargin
    val json = JSON.parseObject(string)
    json should be (JObject(List(
      ("k1", JString("stringval")),
      ("k2", JInt(1234)),
      ("k3", JBool(true)),
      ("k4", JDouble(1234.5678d))
    )))
  }

  it should "be able to parse simple object with arrray" in {
    val string =
      """ {"k1" : "stringval",
        |"k2" : 1234,
        |"k3" : true,
        |"k4" : 1234.5678,
        |"k5" : [1,2,3,4]}""".stripMargin
    val json = JSON.parseObject(string)
    json should be (JObject(List(
      ("k1", JString("stringval")),
      ("k2", JInt(1234)),
      ("k3", JBool(true)),
      ("k4", JDouble(1234.5678d)),
      ("k5", JArray(List(JInt(1), JInt(2), JInt(3), JInt(4))))
    )))
  }

  it should "be able to parse simple object with nested object and array" in {
    val string =
      """ {"k1" : "stringval",
        |"k2" : 1234,
        |"k3" : true,
        |"k4" : 1234.5678,
        |"k5" : {
        | "k5_1" : 1,
        | "k5_2" : [true]
        |}}""".stripMargin
    val json = JSON.parseObject(string)
    json should be (JObject(List(
      ("k1", JString("stringval")),
      ("k2", JInt(1234)),
      ("k3", JBool(true)),
      ("k4", JDouble(1234.5678d)),
      ("k5", JObject(List(
        ("k5_1", JInt(1)),
        ("k5_2", JArray(List(
          JBool(true))))
        )))
    )))
  }


  it should "parse empty json" in {
    JSON.parseObject("{}") should be (JObject(List()))
  }

  it should "parse empty array" in {
    JSON.parseArray("[]") should be (JArray(List()))
  }

  it should "parse json with new lines, empty array, empty nested objects" in {

    val emptyJSON =
      """{  "key1"  : "value1" , "key2": "value2", "array": [
        |
        |
        |
        | ], "eo" : {
        |} }   """.stripMargin

    JSON.parseObject(emptyJSON) should be (JObject(List(
      ("key1", JString("value1")),
      ("key2", JString("value2")),
      ("array", JArray(List())),
      ("eo", JObject(List()))
    )))
  }

  "JSONBuilder" should "create json for simple object" in {
    JSON.json(JObject(List(
      ("k1", JString("stringval")),
      ("k2", JInt(1234)),
      ("k3", JBool(true)),
      ("k4", JDouble(1234.5678d))
    ))) should be ("""{"k1":"stringval","k2":1234,"k3":true,"k4":1234.5678}""")
  }

  it should "create json for simple object with array" in {
    JSON.json(JObject(List(
      ("k1", JString("stringval")),
      ("k2", JInt(1234)),
      ("k3", JBool(true)),
      ("k4", JDouble(1234.5678d)),
      ("k5", JArray(List(JInt(1), JInt(2), JInt(3), JInt(4))))
    ))) should be ("""{"k1":"stringval","k2":1234,"k3":true,"k4":1234.5678,"k5":[1,2,3,4]}""")
  }

  it should "create json for nested object and nested array" in {
    JSON.json(JObject(List(
      ("k1", JString("stringval")),
      ("k2", JInt(1234)),
      ("k3", JBool(true)),
      ("k4", JDouble(1234.5678d)),
      ("k5", JObject(List(
        ("k5_1", JInt(1)),
        ("k5_2", JArray(List(
          JBool(true))))
      )))
    ))) should be ("""{"k1":"stringval","k2":1234,"k3":true,"k4":1234.5678,"k5":{"k5_1":1,"k5_2":[true]}}""")
  }

  "ParseContext" should "read string correctly" in {
    val ctx = new JSONParseContext(""""x\"yz"""")
    ctx.skipSpaces()
    ctx.readString() should be ("x\"yz")
  }

  it should "read string and non string with spaces before and after it" in {
    val ctx = new JSONParseContext(
      """
        |   "xyz" 1234 567, aaa""".stripMargin)
    ctx.skipSpaces()
    ctx.readString() should be ("xyz")
    println("Readstring passed")
    ctx.peek() should be (" 1234 567, aaa")
    println("peek passed")
    ctx.skipSpaces()
    ctx.peek() should be ("1234 567, aaa")
    ctx.readNonString() should be ("1234 567")
    ctx.peek() should be (", aaa")
  }

  //TODO: Cover all data types
  it should "read simple property correcty" in {
    implicit val ctx = new JSONParseContext(""" "key1" : 1234}""")
    JSON.readProperty should be (("key1", JInt(1234)))
  }

  it should "read object corectly" in {
    println("Test : read object corectly")
    implicit val ctx = new JSONParseContext("""{ "key1" : true , "k2" : "xc,vf" }""")
    JSON.readObject should be (JObject(List(("key1", JBool(true)), ("k2", JString("xc,vf")))))
  }

  it should "read array corectly" in {
    println("Test : read array corectly")
    implicit val ctx = new JSONParseContext("""{ "key1" : [1, 2 , 3, {"k2": 4}] }""")
    JSON.readObject should be (JObject(List(("key1", JArray(List(JInt(1), JInt(2), JInt(3), JObject(List(("k2", JInt(4))))))))))
  }

  it should "read nested object corectly" in {
    println("Test : read nested object corectly")
    implicit val ctx = new JSONParseContext("""{ "key1" : 1.2345 , "k2" : { "k2_1" : "<1234>" } }""")
    JSON.readObject should be (JObject(List(("key1",JDouble(1.2345d)), ("k2", JObject(List(("k2_1", JString("<1234>"))))))))
  }
  it should "able to parse externally generate json using http://www.json-generator.com/" in {
    val json = scala.io.Source.fromFile("src/test/resources/generated.json").mkString
    implicit val ctx = new JSONParseContext(json)
    JSON.readValue
    //println(s"Parsed external json is ${JSON.readValue}")
  }

  //Other test-cases : empty array, wrong json etc etc
  //Fails because parser does check for syntax assumes JSON is correct
  //TODO : Add more tests

}
