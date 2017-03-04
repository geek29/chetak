
# Chetak

Chetak is small NIO based *API* server which is written from scratch without using any JVM library or framework. Its ultimate vision is to be "Backend As Service" server with basic service implemented out of the box and user can add many other services when needed 

## Why Chetak

I wrote chetak mostly as learning different aspects of Scala language as well as way to implement basic things (which we daily use but hardly go through how its implemented) like network server, json parsing library in order to learn different areas etc

## Components

Chetak is made of following components

- NIO Server and Http protocol implementation
- Web framework (Http Request Handler API) (almost clone of Spray Framework)
- JSON Parser and builder
- BaS(Backend as Service) Application

## How compliant Chetak is to Http standards

Its not compliant to any http versions right now but slowly I am planning add different aspects of http protocol. But it will never be fully compliant as scope of this project to limit it to only API needs ie dynamic content

## Progress
Currently basic NIO/Http server, JSON Parser, Http Parser and Builder, Http Request handler API is completed with some test coverage

## Meaning of word "chetak"

Chetak was the war-horse of Maharana Pratap, whom Pratap rode during the Battle of Haldighati, June 21, 1576. Pratap and Chetak attacked war elephant of opponent commander head-on in order to kill him. Chetak died in this battle after leaving Pratap out of danger after the failed attack and since. Chetak has been immortalized in the ballads of Rajasthan.

### TODOs
- Write sample service using httpRequestMatcher - KeyValueService
- Add Http Request body types - FormBody and JSON Body
- Add keep-alive support in http
- Add content-types in http
- Add content encodings, media types, url encodings, 
- Add HTTP security

Baas
- API Keys
- API Versioning
- basic entity api similar to parse
- entity storage - in-memory
- entity sync framework
- entity storage - on disk using persistence api
- File storage (images mostly) 
- Identity provider