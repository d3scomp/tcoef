# About

This is a companion page to a paper submitted to ECSA 2017, titled “ADL for Dynamic Coalitions in Smart Cyber-Physical Systems”. It presents an overview of the TCOF library, including short descriptions of the core TCOF concepts and their realization in Scala. In addition, an extended discussion of the advantages and disadvantages of internal vs. external DSL representation is provided, along with the experience gained from developing the external ensemble-based DSL (EDL).

# Project structure
The project consists of the following parts, identified by their path in the folder structure:

`/tcof/src/main/scala/tcof` – core library  
`/tcof/src/main/scala/tcof/traits` – reusable traits  
`/tcof/src/main/scala/rcrs/` - code specific for RCRS domain  
`/tcof/src/main/scala/rcrs/scenario/ProtectScenario` – example scenario with FireFighters  

# Main concepts

![Simplified tcof class diagram](https://github.com/d3scomp/tcof/blob/master/doc/tcof-class-diagram.png "Simplified tcof class diagram")
_Fig. 1 - Simplified tcof class diagram_

The class diagram in Fig. 1 presents a simplified overview of the tcof library. Some classes/traits are intentionally merged into single class in the diagram (e.g., Ensemble, EnsembleGroup, EnsembleGroupMembers traits are represented by the Ensemble abstract class) for the sake of brevity. The library also contains more non-core reusable traits (tcof.traits) than shown in the diagram.

tcof package contains the following core traits/classes:

_Component_ – represents an autonomic entity which has state and performs periodic activity. Component has sensing, constraints, utility and coordination methods, each taking expression (by-name parameter) and being executed by the tcof library in the following order:
1.	In sensing component senses data from the environment and updates its knowledge model.
2.	State (see below) of the component is resolved so that constraints are satisfied and the objective function described in utility is maximized.
3.	Actuation is performed in coordination.

_State_ – a hierarchical structure representing the current status of a component. Whether a component is in a particular state is determined during constraints resolution. A component can be in multiple states at once, i.e., it can be attempting to perform several activities at the same time. Composite states are implemented using StateSetOr, StateSetAnd classes (subclasses of State, omitted from the diagram). For StateSetOr at least one child state must be satisfied, for StateSetAnd all of them must be satisfied.

_Ensemble_ – represents shared state or a joint goal of a group of components. Ensemble is periodically determined at runtime based on its membership condition (specified in membership method) – a predicate defined over the components’ knowledge – so that the objective function (given in the utility method) is maximized. Current implementation uses a component chosen in advance to establish the ensemble (initiator), and host ensemble resolution. 
Ensembles form a hierarchical structure - members of a sub-ensemble must be members of the parent ensemble too. Ensembles are generally allowed to overlap.

_Role_ – models responsibility assigned to a component within an ensemble. Role methods (e.g. all, some, contains, sum…) can be used within Ensemble.membership and Ensemble.utility to describe constraints of the ensemble. Similar to ensembles, roles also form hierarchical structure and can be therefore decomposed/aggregated.

_Model_ - represents a system based on periodical computation. Model class serves as a base for extension by user scenarios (e.g. FireFightingScenario) – non-core traits (e.g. StateSpaceTrait) usually require that the class they are mixed into extends the Model class.

# External vs. internal DSL development approach

In parallel with TCOF-ADL, we have designed a similar language but this time as an external DSL, i.e., an independent language (while TCOF-DSL is an internal DSL, i.e., embedded in the Scala language). This DSL, dubbed the Ensemble Definition Language (EDL for short), is implemented with help of the Java DSL development stack – the Xtext and XTend technologies, as well as Ecore-based modelling tools have been used. As an example, we use the ProtectionTeam ensemble from the accompanying paper – an ensemble consisting of 2 to 3 firefighter brigades assigned to a certain fire location (e.g. a building). Fig. 2 shows the realization of such ensemble in TCOF-ADL, whereas Fig. 3 presents an EDL description with analogous functionality. EDL supports a similar set of concepts as TCOF-ADL, including ensembles, components, utility function (called fitness in the EDL) and membership definition – though ensemble nesting is not allowed.

![tcof DSL example](https://github.com/d3scomp/tcof/blob/master/doc/tcof-code.png "TCOF-ADL example")
_Fig. 2 - ensemble in TCOF-ADL notation_

![EDL DSL example](https://github.com/d3scomp/tcof/blob/master/doc/edl-code.png "EDL-ADL example")
_Fig. 3 - ensemble in EDL-ADL notation_

Compared to the internal-DSL approach, the external-DSL way has both pros and cons.
External DSL pros. Due to having a separate compilation step and working with the model of the ensemble description instead of just data, the external DSL is directly capable of reflective code generation – it is therefore potentially more powerful. Additionally, because the external language design is not bound by the restrictions imposed by a host language, some concepts can be captured more naturally. An example of this can be seen in the role declaration in an ensemble type. Whereas declaring a role in the internal DSL requires passing the name of the role as a string constant (a redundancy, as the name is also used in the field declaration), in the EDL the same declaration can be realized very concisely and intuitively. Of course, this important advantage can also hold a hidden disadvantage – any users of the EDL need to learn and understand a new language. However, as the users need to understand the key concepts of the approach (ensembles, roles, etc.) to use it anyway, they may in the end find the effort of learning a relatively straightforward language for capturing these new concepts a minor investment. Finally, due to being a separate language, the external DSL could conceivably be mapped to different host languages and platforms, generating code for the .NET platform, C++ implementations, etc. This very same language neutrality can also be used to support interoperability of components running on various technology stacks in a single system, allowing for truly heterogeneous deployments.

External DSL cons. While also being the main reason for lacking the advantages discussed in the previous section, the TCOF-ADL’s tight integration with the Scala language allows for a high degree of flexibility, as the user can easily take advantage of the full JVM stack just by writing standard Scala code. Even though supporting Java class libraries could also be done in the EDL, it would take a lot more effort compared to the internal DSL, where all the required language facilities are already in place. A very important difference between the approaches is the ease of extending the framework – the internal DSL approach, having access to powerful features such as traits, makes introducing domain-dependent concepts much easier, as seen in TCOF. For the external DSL, the concept of extensibility must be introduced manually, with EDL opting for function-based extensibility, rather than supporting traits (which would take more time and effort). For this reason, all the domain-dependent concepts are captured via calls to functions that have to be defined in the underlying language. 

To continue with the external DSL cons, for internal DSL, the features such as code completion and type checking are already present and supported by the toolset, usually in a very robust way compared to any custom implementation. Providing a similar set of tools for the external DSL would once again require a lot more effort. Finally, taking advantage of such complete ecosystem also means that the development cycle for new features can be faster, the tooling support is more widespread, and the user does not need to learn or use any new tools – provided he has experience with the base language itself.

Overall, the external DSL approach potentially allows for more expressivity and power, as well as user comfort, but at the cost of a fairly high initial investment and more effort needed for implementing all the necessary features. Utilizing an internal DSL on the other hand results in faster development cycle, and the tight integration with the host language makes implementing domain-specific functionality a lot easier. The points raised in this section are summarized in table XXX

External DSL | Internal DSL
--- | ---
Additional compilation step, possibility of code generation | Easier to integrate – direct access to the host language
Language design can be more flexible, tailored to the problem | Faster development cycle
Can be platform and language neutral | No need to learn new tools or language
Can support heterogeneous deployments	| Much lower initial investment

# Experience from DSL development

Our experience with external DSL development is mostly in line with the general points discussed in the previous sections. For EDL, we have chosen the Xtext suite DSL development approach, so we had to invest quite some time into learning this technology. No matter the technology stack chosen however, this initial investment will be present to some extent, slowing the overall development progress – going down the internal DSL route with TCOF proved to result in faster prototyping.

Designing the language in Xtext was also fairly demanding in terms of theoretical knowledge and expertise. As the textual representation of the Xtext grammar is processed by the ANTLR framework, this occasionally results in conflicts (or ambiguities) within the grammar, with very few pointers on how to resolve them. The language developer therefore needs to have at least a basic understanding of how parsers work, and how to resolve the more common problems within the grammar. This is to be expected when developing an external DSL, and it is a price to pay for the higher flexibility in language design. For the TCOF, the infrastructure of the host language (Scala) was already in place, and therefore again required less time and effort – though this is of course only true as long as the language features map well to host language constructs.


