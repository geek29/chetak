package chetak.json

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by tushark on 4/3/17.
  */
class JSONQuerySpec extends FlatSpec with Matchers {

  "JSON" should "return simple path with 1 level nesting" in {
    val jsonString = """{
          "key1" : {
            "key1_1" : 12345,
            "key1_2" : "stringValue"
          } ,
          "key2" : 1234,
          "key3" : false,
          "key4" : [1,2,3,4]
        }
      """.stripMargin
    val json = JSON.parseObject(jsonString)

    jsonQuery(json, JSONROOT / "key1") should be (Some(JObject(List(("key1_1",JInt(12345)), ("key1_2",JString("stringValue"))))))
    jsonQuery(json, JSONROOT / "key2") should be (Some(JInt(1234)))
    jsonQuery(json, JSONROOT / "key3") should be (Some(JBool(false)))
    jsonQuery(json, JSONROOT / "key4") should be (Some(JArray(List(JInt(1), JInt(2), JInt(3), JInt(4)))))
    jsonQuery(json, JSONROOT / "xyz") should be (None)
  }

  "JSON" should "return simple path with N level nesting" in {
    val jsonString = """{
          "key1" : {
            "key1_1" : {
              "key1_2" : {
                "key1_3" : {
                  "key1_4" :  "value"
                }
              }
          }
        }
      }""".stripMargin
    val json = JSON.parseObject(jsonString)


    jsonQuery(json, JSONROOT / "key1") should be (Some(JObject(List(("key1_1",JObject(List(("key1_2",JObject(List(("key1_3",JObject(List(("key1_4",JString("value")))))))))))))))
    jsonQuery(json, JSONROOT / "key1" / "key1_1") should be ((Some(JObject(List(("key1_2",JObject(List(("key1_3",JObject(List(("key1_4",JString("value")))))))))))))
    jsonQuery(json, JSONROOT / "key1" / "key1_1" / "key1_2") should be (Some(JObject(List(("key1_3",JObject(List(("key1_4",JString("value")))))))))
    jsonQuery(json, JSONROOT / "key1" / "key1_1" / "key1_2" / "key1_3") should be (Some(JObject(List(("key1_4",JString("value"))))))
    jsonQuery(json, JSONROOT / "key1" / "key1_1" / "key1_2" / "key1_3" / "key1_4") should be (Some(JString("value")))

  }

}
