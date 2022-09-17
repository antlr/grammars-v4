
# About ASL

The Action Specification Language (ASL) was originally specified by Kennedy Carter for use as an action language in state models in the Shlaer-Mellor methodology. With the emergence of the Unified Modelling Language (UML) as an industry standard for modelling, ASL was modified to be used in UML state models.

We have used ASL extensively over a number of years in our software designs and this ANTLR grammar has been used to create an ASL parser. We have used this parser to ensure that our ASL is syntactically correct and consistent, hence enabling automatic code generation.

# ASL Terminology

As our use of ASL originates from the Shlaer-Mellor days, we use the earlier Shlaer-Mellor terminology rather than UML in our designs. This has carried through to our ASL grammar but the terms used in Shlaer-Mellor are largely synonymous with UML, as detailed below:

 | SM Term      | UML Equivalent |
 | -------      | -------------- |
 | Domain       | Package        |
 | Object       | Class          |
 | Attribute    | Attribute      |
 | Instance     | Object         |
 | Event        | Signal         |
 | Relationship | Association    |

# Use of ASL for Bridges

The Shlaer-Mellor methodology included bridges as a means of communication between domains but did not define how these would work so we came up with our own method for this. In our methodology, bridges comprise of event mappings between domains and bridge operations.

For the event mappings, a source domain generates an external event containing information that is relevant to one or more other domains which are unknown to the sender. Bridges can then map this external event to internal events in one or more destination domains, translating between the languages of the two domains.

Bridge operations allow a client domain to ask a question of a server domain, without knowing details of the server domain. The bridge will gather the required information to answer the question from a server domain, again translating between the languages of the domains.

This grammar includes constructs for external events and bridge operations which are not part of the ASL as specified by Kennedy Carter.

# Other Differences from the ASL Specification

Our implementation of ASL largely adheres to the ASL specification, but with some modifications which are detailed below.

* Multivalued associative relationships were not used as it was felt that such situations were better to be modelled as an associative relationship with a separate association. Therefore the “associate” and “unassociate” contsructs were not used and are not present in the ASL grammar.

* Relationship role phrases on a class diagram were defined by Kennedy Carter as space separated words, for example “is owned by”. However, in association navigation spaces are replaced by underscore, e.g. R3.”is_owned_by”. Our grammar expects spaces rather than underscores in relationship navigation though.

* For sending of events, we use the keyword “%generate” rather than “generate” and this is reflected in the grammar.

* For timer events, in our implementation we specify the target timer differently to in the ASL reference guide. For example we do:

>   %generate TIM2:Reset_Timer(); to this.Timer_id
>
> but in the ASL reference guide this is given as:
>
>   generate TIM2:Reset_timer(this.Timer_id)

* In a TIM1 event (set timer), the ASL reference guide includes a time granularity data item but our implementation doesn’t and just defaults to a granularity of seconds.

* In a TIM10 event (set absolute timer), the ASL reference guide has a date parameter and a time parameter. We have separate parameters for year, month, date, hour, minute and seconds.

* Our implementation includes an additional timer event which is used to set a chimer timer as follows:

>   %generate TIM3:Set_Chimer(<start_time>, <repeat_period>, <event>, <return instance handle>); to <timer_id>
>
> where:
>
>   <start_time> is the time in seconds when the chimer should first fire
>   <repeat_period> is the period in seconds at which the chimer should repeat
>   <event> is the return event to be sent when the timer goes off
>   <return instance handle> is a handle of an instance to which the return event should be sent
>   <timer_id> is the id of the timer to be set

* Our grammar enforces a naming convention for entities in our design to ensure that our use of ASL is consistent. Our naming conventions are as follows:

 | Item            | Naming Convention                               |
 | --------------- | ----------------------------------------------- |
 | Object names    | Uppercase words separated by underscore         |
 | Attribute names | A leading uppercase word, optionally followed   |
 |                 | by uppercase or lowercase words separated by    |
 |                 | underscore                                      |
 | Event names     | Leading uppercase words separated by underscore |
 | Operation names | Leading uppercase words separated by underscore |

