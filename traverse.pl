:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(hash_stream)).
:- use_module(library(c14n2)).
:- use_module(library(memfile)).
:- use_module(library(sgml)).
:- use_module(library(uuid)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(record)).
:- use_module(library(option)).

traversable_structures(Options, Traversable_Structures) :-
    %  Merge Options with Defaults
    make_options(Options, Unified_Options),
    options_traversable_structures(Unified_Options, Traversable_Structures).

transformable_structures(Options, Transformable_Structures) :-
    %  Merge Options with Defaults
    make_options(Options, Unified_Options),
    options_transformable_structures(Unified_Options, Transformable_Structures).

%!	traverse(+List, +Options, -List).
:-  record options(
            traversable_structures      :list=[_], 
            transformable_structures    :list=[_]
           % transform_structures_via    :callable=true
    ).

traverse([FirstNode | RemainingNodes], Options, Output_Traversed_List) :- 
    traversable_structures(Options, Traversable_Structures),
    memberchk(FirstNode, Traversable_Structures) -> traverse(FirstNode, Options, Traversed_FirstNode),
    traverse(RemainingNodes, Options, Traversed_RemainingNodes),
    Output_Traversed_List = [ Traversed_FirstNode | Traversed_RemainingNodes ],
    !.

traverse([], Options, Output_Node) :- Output_Node = [], !.

traverse([Term], Options, Output_Node) :- 
    traverse(Term),
    Output_Node = [Term], !.

traverse(A=B, Options, Output_Node) :- transform((A=B), Options, Output_Node), !.

traverse(Term, Options, Output_Term) :- 
    (   atom(Term) 
    ->  transform(Term, Options, Output_Term)
    ;   traverse_compound(Term, Options, Output_Term)
    ), !.

traverse_compound(Compound, Options, Output_Term) :-
    compound_name_arguments(Compound, Functor, CompoundArguments),
    transformable_structures(Options, Transformable_Structures),
    memberchk(Functor, Transformable_Structures) -> transform(Functor, Options, Transformed_Functor),
    memberchk(CompoundArguments, Traversable_Structures) -> traverse(CompoundArguments, Options, Traversed_CompoundArguments),
    compound_name_arguments(Compounded_Output_Term, Transformed_Functor, Traversed_CompoundArguments),
    Output_Term = Compounded_Output_Term.

%!	traverse(+List, +Options, -List).
:-  record transformOptions(
    transformable_structures    :list=[_],
    transform_structures_via    :callable=!
    ).

transform(Term, Options, Output) :-
    Output = Term.
 /* % Initial Clause Filter e.g. foo(bar,_,_,baz), [a,b,_] ?
    (memberchk(call(Function), Options)
        -> !; !.
    ),

   % If there's a transformer function, call it.
    (memberchk(call(Function), Options) 
        % Can we call this? 
        -> catch(call(Function, Term, Output),
                    error(Err,_Context),
                    true % format('You done goofed! ~w\n', [Err])
                ), !
        ; Term = Output, !
    ), Term = Output, !.
*/

% TODO 
uniquify_xml_node_via_id(Node) :- !.

% TODO 
normalize_element([],_).
normalize_element(element(Type, Attributes, Children), NormalizedElement) :- 
    writeln('Normalizing Element...'),
    writeln(Type), 
    writeln(Children),
    normalize_element(Children, NormalizedElement).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

% Test XML

:- load_xml('./_temp/Sample_RegA_1A.xml', XmlDocument, []),
    assertz(xml_document(XmlDocument)).

% Test HTML
% ?- html:document(X), traverse(X,[],Y).
% [element(html,[donuts=tasty],[element(head,[],[element(title,[],[hi])]),element(body,[],[])])].

:- open_string('
    <html donuts="tasty">
        <head >
            <title>hi</title>
        </head>
        <body></body>
    </html>', Stream),

    dtd(html, DocumentType),
    load_structure(Stream, DomDocument, [DocumentType]),
    assertz(document(DomDocument)),
    /*
    normalize_element(DocumentElement, NormalizedElement),
    sort_element_attributes(DocumentElement, SortedElement),
    % uniquify_xml_node_via_id ...
    */

    close(Stream).
