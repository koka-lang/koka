/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/
module chatbot

import std/readline

val answers = [ 
  "In what way?",
  "Can you think of a specific example?",
  "Your boyfriend made you come here?",
  "I am sorry to hear you are depressed.",
  "What would it mean to you if you got some help?",
  "Who else in your family takes care of you?",
  "What resemblance do you see?",
  "Does that have anything to do with the fact that your boyfriend made you come here?",
  "What else comes to mind when you think of your father?",
  "Does it please you to believe I am afraid of you?",
]

public fun main() : <console,exn,ndet,div,async> () 
{ 
  fun reacton(input: string) : <console,exn,ndet,div,async> ()
  {
    if (input != "exit" && input != "quit") {
      val answer = answers[random-int() % answers.length()].maybe("?",id)
      question(answer).reacton
    }
  }

  reacton( question("(type 'exit' to quit)\n\nWhat is your concern?" ) );
}
