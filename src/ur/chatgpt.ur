open Json
open Urls

functor TwoLegged(M : sig
                        val api_token : string
                    end) = struct
    open M

    val token = return (Some api_token)
end

datatype role = System | User | Assistant
type role' = variant [System = unit, User = unit, Assistant = unit]

val read_role = mkRead' (fn s =>
                            case s of
                                "system" => Some System
                              | "user" => Some User
                              | "assistant" => Some Assistant
                              | _ => None)
                        "role"

val show_role = mkShow (fn x =>
                           case x of
                               System => "system"
                             | User => "user"
                             | Assistant => "assistant")

val json_role : json role = @json_derived Result.readResult show _


type message = {
     Role : role,
     Content : string
}
val _ : json message = json_record
                           {Role = "role",
                            Content = "content"}

type choice = {
     Message : message
}
val _ : json choice = json_record
                      {Message = "message"}

type response = {
    Choices : list choice
}
val _ : json response = json_record
                            {Choices = "choices"}

type model = string
val read_model = _
val show_model = _

datatype response_format = AsJson | RegularText
type response_format' = variant [AsJson = unit, RegularText = unit]

val read_response_format = mkRead' (fn s =>
                            case s of
                                "json_object" => Some AsJson
                              | "text" => Some RegularText
                              | _ => None)
                        "response_format"

val show_response_foramt = mkShow (fn x =>
                           case x of
                               AsJson => "json_object"
                             | RegularText => "text")

val json_response_format : json response_format = @json_derived Result.readResult show _

type response_format_dict = {Type : response_format}

val _ : json response_format_dict = json_record {Type = "type"}

type completions_arg = {
     Model : model,
     Messages : list {Role : role,
                      Content : string},
     ResponseFormat: response_format_dict
}
val _ : json completions_arg = json_record
                                   {Model = "model",
                                    Messages = "messages",
                                    ResponseFormat = "response_format_dict"}

functor Make(M : sig
                 val token : transaction (option string)
             end) = struct
    val token =
        tok <- M.token;
        case tok of
            Some tok => return tok
          | None => error <xml>How odd: no ChatGPT token!</xml>

    val urlPrefix = "https://api.openai.com/v1/"

    fun api url body =
        tok <- token;
        WorldFfi.post (bless (urlPrefix ^ url)) (WorldFfi.addHeader WorldFfi.emptyHeaders "Authorization" ("Bearer " ^ tok)) (Some "application/json") body

    structure Chat = struct
        fun completions arg =
            r <- api "chat/completions" (toJson arg);
            case (fromJson r : response).Choices of
                {Message = {Content = choice, ...}, ...} :: [] =>
                return choice
              | _ => error <xml>Unexpected number of choices in ChatGPT response</xml>
    end
end