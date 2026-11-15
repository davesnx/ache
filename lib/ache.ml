module Headers = Headers
module IOVec = IOVec
module Runtime = Runtime
module Method = Method
module Reqd = Reqd
module Request = Request
module Response = Response
module Status = Status
module Version = Version
module Body = Body
module Config = Config
module Server_connection = Server_connection
module Client_connection = Client_connection

module Ache_private = struct
  module Faraday = Faraday

  module Runtime = struct
    module Upgradable = Runtime.Upgradable
    module Buffer = Runtime.Buffer
  end

  module Respd = struct
    type t = Respd.t

    let awaiting request response_handler =
      let writer = Serialize.Writer.create () in
      let respd =
        Respd.create
          ~error_handler:ignore
          ~writer
          ~response_handler
          request
          (Body.Writer.create_empty ~writer)
      in
      Respd.write_request respd;
      respd
  end

  module Parse = Parse
  module Serialize = Serialize
end
