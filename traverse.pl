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

%!	traverse(+List, +Options, -List).
:-  record options(
            traversable_structures      :list=[_], 
            transformable_structures    :list=[_]
            transform_structures_via    :callable=!,

    ).

traverse([FirstNode | RemainingNodes], Options, Output_Traversed_Transformed_List) :- 
    %  Merge Options with Defaults
    make_options(Options, Unified_Options),
    options_traversable_structures(Unified_Options, Traversable_Structures),
    writeln(Traversable_Structures),
    writeln(FirstNode),
    %   Check if Term is Traversable
    (   memberchk(FirstNode, Traversable_Structures) 
    ->  traverse(FirstNode, Options, Traversed_FirstNode)
    ;   !
    ),
    traverse(RemainingNodes, Options, Traversed_RemainingNodes),
    %   Combine FirstNode and RemainingNodes into an Output List
    Output_Traversed_Transformed_List = [ Traversed_FirstNode | Traversed_RemainingNodes],
    !.

traverse(Term, Options, Output_Term) :- 
    % Merge Options with Defaults
    make_options(Options, Unified_Options),
    options_traversable_structures(Unified_Options, Traversable_Structures),

    % Establish Transformation Options
    options_transformable_structures(Unified_Options, Transformable_Structures),
    options_transform_structures_via(Unified_Options, Transform_Structures_Via),
    Transformation_Options = [Transformable_Structures, Transform_Structures_Via]

    %   Check if Term is Traversable
    (   atom(Term)
    ->  compound_name_arguments(Term, Functor, CompoundArguments),
    ;   

    /*(   memberchk(Term, Traversable_Structures)
    ->
    ; !  
    )
    */
  /*  (   memberchk(Term, Traversable_Structures)
    ->  true (   compound(Term),
        ->
            % Unfortunately, if->then doesn't use blocks!

            compound_name_arguments(Term, Functor, CompoundArguments),
            %%%%%%%% TODO: Provide parent context to Options.. in this case, compound(Term).
            ParentContext_Provisioned_Options = Options,
            % Pass compound(Term) to  ParentContext_Provisioned_Options(ContextType? ContextFunctorTerm?)
            traverse(CompoundArguments, CompoundArgument_Flagged_Options, Traversed_CompoundArguments),
            % We can reassemble with... ?Term =.. ?List....  ?- Term =.. [functor, a(b)].  Term = functor(a(b)) 
            % This is where we need to flag the term's type (it's a compound)
            Functor_Flagged_Options = Options,
            %
            transform(Term, Functor_Flagged_Options, TransformedTerm);
            % Pass the original Options if it's not a Compound.
            transform(Term, Options, TransformedTerm)
        ;
            !
        )
    ;
        !
    ), */
    Output_Term = Term,
    !. 


%!	traverse(+List, +Options, -List).
:-  record transformOptions(
    transformable_structures    :list=[_]
    transform_structures_via    :callable=!,

).

transform(Term, Options, Output) :-
    writeln('Transforming: '), print(Term),
/*  % Initial Clause Filter e.g. foo(bar,_,_,baz), [a,b,_] ?
    (memberchk(call(Function), Options)
        -> !; !.
    ),
*/
    % If there's a transformer function, call it.
    (memberchk(call(Function), Options) 
        % Can we call this? 
        -> catch(call(Function, Term, Output),
                    error(Err,_Context),
                    true % format('You done goofed! ~w\n', [Err])
                ), !
        ; Term = Output, !
    ), Term = Output, !.

uniquify_xml_node_via_id(Node) :- !.
/*
assert_node_has_unique_attribute(Attribute, element(Type, Attributes, Children), UniqueElement) :- 
    uuid(UniqueIdentifier),
    writeln(Type),
    writeln('Attributes:'),
    writeln(Attributes),
    UniqueElement = element(Type, Attributes, Children).
*/

normalize_element([],_).
normalize_element(element(Type, Attributes, Children), NormalizedElement) :- 
    writeln('Normalizing Element...'),
    writeln(Type), 
    writeln(Children),
    normalize_element(Children, NormalizedElement).

:- load_xml('./_temp/Sample_RegA_1A.xml', XmlDocument, []),
    assertz(xml_document(XmlDocument)).
    /* writeln(XmlDocument), */

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
    normalize_element(DocumentElement, NormalizedElement),*/
    % sort_element_attributes(DocumentElement, SortedElement),
    /* writeln(SortedElement), */
    close(Stream).

   %  assign_element_unique_attribute('id', DocumentElement, UniqueDocumentElement),
    /* element_has_attribute('id', DocumentElement), */
    /* ( 
       
        % yes

     %    ;
        %no
     )

     assign_element_unique_attribute('id', DocumentElement, UniqueDocumentElement),
