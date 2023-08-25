open Serde
open Bechamel
open Toolkit
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let big_string =
  {|["

How I explore domain problems faster and cheaply with OCaml: modeling a web router
You've heard of Domain-Driven Design, now buckle up for Type-Driven Domain..wait. Typed Domains Driving...nevermind. We're gonna use Only Types to Understand our Domain Problems Very Fast! 🚀

Leandro Ostera
Leandro Ostera
Aug 24, 2023 • 9 min read
Hello folks! Starting out the blog with a topic that I love: domain modeling.

Domain modeling is the art and science of figuring out how to map some messy, fuzzy, real-life ideas and things, into a computer program that we can execute.

It is usually easier to say than it is to do, so I figured I'd give you an example of how I've done domain modeling in the past and how I like to explore domain problems through it.

Shapes of Things
There's only so many kinds of data we can have in our programs. You have single things, you have collections of the same things. You have collections of different things that also happen to be a thing, and they can either be one of many things that are the same thing, or many things together making a thing!

Where I'm going with this is that the shapes of data that you normally work with come in a few packages.

We have many things that belong together but are distinct on their own. In OCaml we call these variants.

We have many things that belong together and form a single unit, where every piece has its own place, and the place doesn't have a name. In OCaml we call these tuples. But when these pieces don't have a place and instead have a name, in OCaml we call these records.

We have things that exist on their own and we don't know much about them. In OCaml we call these abstract types.

We have things that don't tell us everything that they are, but that can hide information from us. In OCaml we call these generalized abstract data types (GADTs, and I pronounce it like \"cats\" but with a G, try it out loud, its cool, no-one can hear you).

We have things that are collections of other things. Lists, arrays, hash tables, sets, queues, heaps, tuples.

And in fact, we can create most of these different shapes of data with some of the simpler ones. Records can made with tuples. Lists can be made with variants. And so on.

Okay, enough of this. Let's start using these shapes for some specific domain problems.

Modeling a Web Router
We'll start with one that most of you will likely be familiar with: a web router. That is a router that helps a web framework figure out where to send each request. There are many out there, and I'm not pitching you to write your own (but you should, because it's a good way to learn!).

But what really is a web router? It's a collection of patterns and handler functions. Not unlike a match expression really. It matches the pattern against a web request object, and if it succeeds it will execute the function that corresponds to it.

So we can start by defining what we know:

there are patterns, and
there are handler functions,
a route is a pair of a pattern and a handler
a router has routes

Brilliant! Now we have our types in place, we can start exploring how they interact with each other.

A router typically will receive some form of request and turn it into a response. After all, we expect to reply to our users with something.

So then, to make a response, we need  to find a handler. We can do that by matching against every route until a pattern matches. When that happens it expects to receive a response.

Now slowly we are building up the right vocabulary not just around the problem, but also in the code that deals with it.


Notice how we create a few new types for request and response, which are new Things we are working with.

We also created two new functions, one for matching a pattern against a request called matches; and a second one called route to create a response from a router and request.

And that's it. We have our first model for a router. We have a clearer understanding of what the moving pieces are, and how they connect together.

From here we can take it in many directions, but what I like to do is to do a second pass and challenge the model.

Challenging the model
In the process of challenging, we want to grab individual pieces and ask what's important about them, and how are they different than other things, and why are they really needed.

For example, why is a pattern a separate entity and not just a behavior of a handler? A handler could well ignore a request and just let the next handler handle it.

This would lead to a slightly different model, where a handler either tells us it has handled or ignored something, or the handler itself calls the next thing.

In the first case, we can model it by making a new type of that can be either an ignored handler result, or a handled handler result:


This leads naturally to some implementations, such as folding over the list of routes, and bailing as soon as we find a route that returned Handled(res). This is super flexible when it comes letting the route itself decide how or if it will process a request.

But in the second case, we can see we have an even more powerful model. In this one we are making sure every handler receives the next handler, which it can call at any point, any number of times. This is what this second model looks like:


This model leads to a recursive implementation, where we have to build our handlers in advance, so that the calls to next go in the right order. This can be much trickier than the prior models we saw.

Refining the Model
Once we find a model that we like, and in this case we'd like to stick with the first one, we can start doing some refinements.

Refining is the process of adding detail to the model, and it helps us see how it can materialize as a working application.

For example, we can take our pattern type and start looking into what shapes it can actually take. Usually, a domain expert here is the best person to ask: \"what really is a pattern?\"

In our case, we want to be able to match on the route URL or path; the kind of HTTP method they are using, or verb; and we'd like to know what is in the body.

To do this we expand our pattern type once to include some data, and in the process we define a new type for the HTTP method, since we knew we needed that and we roughly understand upfront the shape it has: it can be one of some options. The new pattern now looks like this:


Excellent, but now what exactly happens in the body?

As it is above, it can either be present and be a string, or be not present. If it is Some(string) then either an empty string \"\" and the entire works of Shakespeare in Korean would be valid bodies. Is this really what we mean to say?

Here's where our refining doubles down, and asks if there's anything special about the body in this specific pattern, or in this specific route. So we're relating the current refining learnings with our past learnings.

Turns out the body should actually be something that the handler can in fact handle, so we need somehow to make it fit into what the handler expects.

So does the handler really expect a request? Or does it expect a specific kind of request? Let's see if we can be more specific in a few steps:

Make our request parametric
Let our pattern be more specific about a request payload
Make our handler be specific about the request payload
Make our router work with our new handler
We will start by making our pattern take type parameter. At this point a type parameter usually means \"here's a kind of data that is really a much larger group of data, where there are subgroups of it that can't be mixed\".


BUT there's a big problem here. We can't really create the pattern ahead of time, knowing what the payload will be like.

A pattern really is a specification for how to match against requests. So instead, we need to provide a way of reading the body into the 'payload type. Thankfully, we have first-class functions in OCaml, so this is an easy fix.


Notice how our body now becomes a function that will receive a string and try to return a 'payload. If it can't, the it can always return an option. In practice we would probably use a result type here, but for the sake of this post an option is good enough.
Next up, we have our handler, which now should receive a type parameter for our payload and look a bit more like this:


Our handler function type now receives a payload before a request.
And finally, it seems that our route and router type doesn't need much amending. Because we really just need a list of patterns and handlers, and that's exactly what they are, right?

Right?! 🙈


Oh no. If we follow this current refinement and thread in a 'payload parameter to our route type, we will end up with a single type of payload in the list of routes. This is because the list type can only hold one type of elements, and every 'payload route is essentially a new type!

unit route is a type of routes that have no payloads
user route is a type of routes that have payloads of type user
and every one of these is not mixable with the rest :(
So we can either backtrack, and move this body parsing function inside the handler, to let the handlers figure out how to work with it. Or we can find another way of putting all these handlers together in a list, while maintaining the model as close to the domain.  

For this, we can use a special type of type OCaml has, that lets us hide information. The gist is this:

We will refactor our route type to include a constructor named Route
this constructor will take a 'payload pattern and a 'payload handler
and it will return a route that hides the 'payload information
so we can put all our routes in a single list!

Using a GADT to capture a route with a pattern and a handler, but hiding their payload parameter. {
Damn, there we go! 🔥 Now we have all the information we want and it seems to be nicely encapsulated in this route type.

This model actually leads to a rather complex implementation, because every time we unpack the Route, we must make use of both the pattern and the handler at the same time. That's the only requirement for using this information-hiding pattern: you can peak, but you can't leak the information.

For completeness sake, here's a small implementation that follows our model:


Conclusions from Modeling a Router
Like this, we've quickly gone through several iterations of our model, tried to understand better what problem we are trying to solve, what are some of the constraints it has, and how our model leads to different implementations.

It is very important to understand that this first implementation is meant to be correct, and not necessarily optimal. But it can make a great first implementation to test things against, and eventually, help you optimize making sure you are not breaking good behavior!

I'd love to go on with some more examples, like:

modeling regulatory compliance for betting companies
modeling the publishability window of content in the music industry following geographic restrictions
modeling the optimal publishing of photography content to a social network
modeling an offline-first graph database for the edge
and more!
But we're already over 2,000 words and I'd like you to get a glass of water and maybe stretch your legs. So let me know which modeling example you'd like to see in a next post.

If you liked this, please subscribe so you get the next issue of Practical OCaml right in your inbox, and share it with your camel friends on lobste.rs, hackernews, x.com, and so on.

And I would love to hear if this is the kind of content you're expecting from a Practical OCaml website, so let me know! I'm on x.com: @leostera

Happy Cameling!
    
    "
    , 1234]
|}

type str = string * int [@@deriving serializer, deserializer, yojson]

type variant =
  | Hello
  | Tuple1 of string
  | Record3 of { name : string; favorite_number : int; location : string }
[@@deriving serializer, deserializer, yojson]

type record_ = { name : string; favorite_number : int; location : string }
[@@deriving serializer, deserializer, yojson]

let string_serde =
  Test.make ~name:"string::serde"
    (Staged.stage @@ fun () -> Serde_json.of_string deserialize_str big_string)

let unit_variant_serde =
  Test.make ~name:"unit_variant::serde"
    ( Staged.stage @@ fun () ->
      Serde_json.of_string deserialize_variant {|"Hello"|} )

let tuple_variant_serde =
  Test.make ~name:"tuple_variant::serde"
    ( Staged.stage @@ fun () ->
      Serde_json.of_string deserialize_variant
        {|{ "Tuple1": ["this is a tuple"] }|} )

let record_variant_serde =
  Test.make ~name:"record_variant::serde"
    ( Staged.stage @@ fun () ->
      Serde_json.of_string deserialize_variant
        {|{ "Record3": ["Benjamin Sisko", 9, "Bajor"] }|} )

let record_serde =
  Test.make ~name:"record::serde"
    ( Staged.stage @@ fun () ->
      Serde_json.of_string deserialize_record_
        {|{ "Record3": ["Benjamin Sisko", 9, "Bajor"] }|} )

let string_yojson =
  Test.make ~name:"string::yojson"
    ( Staged.stage @@ fun () ->
      let yojson = Yojson.Safe.from_string big_string in
      str_of_yojson yojson )

let unit_variant_yojson =
  Test.make ~name:"unit_variant::yojson"
    ( Staged.stage @@ fun () ->
      let yojson = Yojson.Safe.from_string {|["Hello"]|} in
      variant_of_yojson yojson )

let tuple_variant_yojson =
  Test.make ~name:"tuple_variant::yojson"
    ( Staged.stage @@ fun () ->
      let yojson = Yojson.Safe.from_string {|["Tuple1","this is a tuple"]|} in
      variant_of_yojson yojson )

let record_variant_yojson =
  Test.make ~name:"record_variant::yojson"
    ( Staged.stage @@ fun () ->
      let yojson =
        Yojson.Safe.from_string
          {|["Record3",{"name":"Benjamin Sisko","favorite_number":9,"location":"Bajor"}]|}
      in
      variant_of_yojson yojson )

let record_yojson =
  Test.make ~name:"record::yojson"
    ( Staged.stage @@ fun () ->
      let yojson =
        Yojson.Safe.from_string
          {|{"name":"Benjamin Sisko","favorite_number":9,"location":"Bajor"}|}
      in
      record__of_yojson yojson )

let () =
  Bechamel_notty.Unit.add Instance.monotonic_clock
    (Measure.unit Instance.monotonic_clock)

let run_bench name tests =
  let benchmark () =
    let tests = Test.make_grouped ~name ~fmt:"%s %s" tests in
    let instances = Instance.[ monotonic_clock ] in
    let cfg =
      Benchmark.cfg ~limit:10000 ~stabilize:true ~quota:(Time.second 0.5) ()
    in
    Benchmark.all cfg instances tests
  in

  let analyze results =
    let ols =
      Analyze.ols ~bootstrap:1 ~r_square:true ~predictors:[| Measure.run |]
    in
    let results = Analyze.all ols Instance.monotonic_clock results in
    Analyze.merge ols [ Instance.monotonic_clock ] [ results ]
  in

  let img (window, results) =
    Bechamel_notty.Multiple.image_of_ols_results ~rect:window
      ~predictor:Measure.run results
  in

  let open Notty_unix in
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> { Bechamel_notty.w; h }
    | None -> { Bechamel_notty.w = 80; h = 1 }
  in
  let results = benchmark () in
  let results = analyze results in
  img (window, results) |> eol |> output_image

let () = run_bench "de" [ string_serde; string_yojson ]
let () = run_bench "de" [ unit_variant_serde; unit_variant_yojson ]
let () = run_bench "de" [ tuple_variant_serde; tuple_variant_yojson ]
let () = run_bench "de" [ record_variant_serde; record_variant_yojson ]
let () = run_bench "de" [ record_serde; record_yojson ]
