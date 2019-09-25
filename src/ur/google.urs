signature S = sig
    val client_id : string
    val client_secret : string
    val https : bool
end

(* First service: merely verifying which Google user we are dealing with *)
functor Login(M : S) : sig
    val authorize : { ReturnTo : url } -> transaction page
    val whoami : transaction (option string)
    (* ^-- Warning: returns an opaque numeric ID, not an e-mail address!
     * Google's API docs encourage use of this ID to prepare for e-mail changes by users. *)
    val logout : transaction unit
end

type message_id
val show_message_id : show message_id
     
type thread_id
val show_thread_id : show thread_id

type message = {
     Id : message_id,
     ThreadId : thread_id
}

type messages = {
     Messages : list message,
     ResultSizeEstimate : int
}
                       
functor Gmail(M : S) : sig
    val authorize : { ReturnTo : url } -> transaction page
    val logout : transaction unit

    val messages : transaction messages
end