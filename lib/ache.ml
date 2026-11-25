module Bigstring = Bigstring
module Headers = Headers
module IOVec = IOVec
module Runtime = Runtime
module Method = Method
module Request_descriptor = Request_descriptor
module Request = Request
module Response = Response
module Status = Status
module Version = Version
module Body = Body
module Config = Config
module Server_connection = Server_connection
module Client_connection = Client_connection
module Websocket = Websocket
module Testing = Testing

module Ache_private = struct
  module Encoder = Encoder

  module Runtime = struct
    module Upgradable = Runtime.Upgradable
    module Buffer = Runtime.Buffer
  end

  module Response_descriptor = struct
    type t = Response_descriptor.t

    let awaiting request response_handler =
      let writer = Serialize.Writer.create () in
      let respd =
        Response_descriptor.create
          ~error_handler:ignore
          ~writer
          ~response_handler
          request
          (Body.Writer.create_empty ~writer)
      in
      Response_descriptor.write_request respd;
      respd
  end

  module Parse = Parse
  module Serialize = Serialize
end
