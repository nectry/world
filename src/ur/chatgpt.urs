functor TwoLegged(M : sig
                        val api_token : string
                    end) : sig
    val token : transaction (option string)
end

(** * API records *)

(* LLM models supported by ChatGPT *)
type model
val read_model : read model
val show_model : show model

datatype role = System | User | Assistant
val read_role : read role
val show_role : show role

(* Different response type we can ask the GPT to return *)
datatype response_format = AsJson | RegularText
val read_response_format : read response_format
val show_response_format : show response_format

(* Messages can be in a variety of forms *)
(* As of 2/21/2024, passing in an image requires using "gpt-4-visual-preview" as the model *)
datatype message
  = PlainText of {Role : role, Content : string}
    (* A plain text message *)
  | UserImageUrl of string
    (* A url to an image that the user wants ChatGPT to reference *)
  | UserImage of string
    (* A base64-encoded image that the user wants ChatGPT to reference *)


(** * Now for the actual methods.... *)

functor Make(
  M : sig
    val token : transaction (option string)
  end) : sig
    structure Chat : sig
      (* Based on the OpenAI "create chat completion" API (documentation found
      here: https://platform.openai.com/docs/api-reference/chat/create).  The
      input requires the ChatGPT model to use, the chat conversation so far
      (including both User prompts and Assistant responses), and a response
      format that indicates whether ChatGPT should return raw text or a
      parseable json result.  The arguments may seem a little fiddly (e.g.,
      ResponseFormat taking a singleton record rather than just a value), but
      this is because they match up with the API more directly. *)
      val completions :
        {Model : model,
         Messages : list message,
         ResponseFormat: {Type: response_format}}
        -> transaction string
    end
end
