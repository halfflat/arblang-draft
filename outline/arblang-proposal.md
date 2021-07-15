# Arblang updated syntax proposal (still WIP)

A mechanism is defined by an interface specification that depends on the sort of mechanism (e.g. point process, density process, concentration model). In the interface block, keywords introduce: bindings of names to cellular state or mechanism state; specification of user-visible parameters; specfication of state evolution.

The expressions that are given in an interface block can be complete in and of themselves, with optional typing (if no types, then types are deduced). But they can also reference function definitions, constants and parameters in one or more named _modules_.

## Encoding

Arblang source is represented in the Unicode character set. After translation from any native encoding, before parsing the source is transformed into Unicode Normalization Form C.

The character representation of tokens is not unique: character sequences that are the same under Unicode NFC normalization are treated as equivalent, and some tokens in addition admit additional representations. In particular:

&lt;multiplication-dot&gt;
  ~ U+00B7 `·` MIDDLE DOT or U+22C5 `⋅` DOT OPERATOR.

&lt;minus-sign&gt;
  ~ U+002D `-` HYPHEN-MINUS or U+2212 `−` MINUS SIGN.

&lt;division-slash&gt;
  ~ U+002F `/` SOLIDUS or U+2215 `∕` DIVISION SLASH.

&lt;micro-prefix&gt;
  ~ U+03BC `μ` GREEK LETTER SMALL MU or U+00B5 `µ` MICRO SIGN or
    U+0075 `u` LATIN SMALL LETTER U

&lt;ohm-symbol&gt;
  ~ U+03A9 `Ω` GREEK LETTER CAPITAL OMEGA or U+2126 `Ω` OHM SIGN.
  
&lt;reaction-right&gt;
  ~ U+2192 `→` RIGHTWARDS ARROW or the sequence `->`.
  
&lt;reaction-left&gt;
  ~ U+2190 `←` LEFTWARDS ARROW or the sequence `<-`.
  
&lt;reaction-rightleft&gt;
  ~ U+21C4 `⇄` RIGHT ARROW OVER LEFT ARROW or the sequence `<->`.

Allowing multiple symbols in these instances allows flexibility in input environments and easier copy/paste from mathematical software. A goal is to allow mathematical and quantity expressions to be represented similarly to how they might be typeset.

## Whitespace

Outside of string literals (see below), all non-empty sequences of whitespace are treated equivalently.

| <whitespace> ::= <whitespace-char>+
| <whitespace-char> ::= any character with the Unicode property `White_Space`.
    
## Identifiers

An identifier is a label used to denote a function, a module, an imported module, a constant, a parameter, a function argument, an external binding, a record field, a record type alias, or a local value binding.

Identifiers have a _scope_. Generally, no matter the sort of scope, an identifer's scope does not precede the point where that identifier is introduced by some declaration or binding. The scopes are as follows:

Global scope
  ~ A module definition introduces the module name into the following global scope. The environment may also introduce module names defined in other arblang sources. 

Module scope
  ~ Parameter, constant, record type aliases, and function definitions have module scope, as do module imports and external bindings. The scope extends from after the definition or import, until the end of the enclosing module or interface definition.

Function scope
  ~ Function arguments have function scope, which extends until the end of the function definition.

Regime scope
  ~ All top-level regime definitions in an interface have scope of the entire interface definition, even before the regime definition. The same applies within a region definition: the scope of an internal region identifier is the entire body of the region definition.

Expression scope
  ~ `let` and `with` binding expressions introduce new identifiers
     that have as scope the final expression in the binding.
    
Valid identifier names are of the form:

> <identifier> ::= <initial-id-symbol> [<id-symbol>]*
> <id-symbol> ::= <initial-id-symbol> | <other-id-symbol>

Identifiers broadly follow the conventions of Python, where below, braces are used to denote Unicode character classes or properties.

> <initial-id-symbol> ::= {L} | {Nl} | {Other_ID_Start}
> <other-id-symbol> ::= {Nd} | {Mn} | {Mc} | {Pc} | <prime-mark>
> <prime-mark> ::= `'` | `ʹ` U+02B9 MODIFIER LETTER PRIME | `′` U+2032 PRIME

&lt;initial-id-symbol&gt; can be any character in the Letter 'L' category, in Number-letter 'Nl' category, or any character with the 'Other_ID_Start' property, which corresponds to the permitted Python starting characters with the exception of the underscore.

&lt;other-id-symbol&gt; includes digits, underscores and similar punctuation, but also &lt;prime-mark&gt;, which is used to denote a derivative.
    
Unlike Python, characters that are only _compatibility_ equivalent to acceptable identifier symbols are not permitted. In particular, superscript numerals are not permitted within an identifier.
    
Two identifiers are equivalent if they have the same canonical representation, which corresponds to the Unicode NFKC normalization with the additional mapping of any character in &lt;prime-mark&gt; to `'`.

### Qualified identifier
    
A qualified identifier is a particular sort of expression that identifies a definition in an imported module, or a sub-regime within a regime.
    
> <qualified-identifier> ::= <identifier> | <qualified-identifier> `.` <identifier>

## Names
    
Some objects can take arbitrary labels rather than identifiers, namely chemical species and interface names. These are represented by string literals:

> <string-literal> ::= `"` [ `\"` | `\\` | <unescaped-char> ]* `"`
> <unescaped-char> ::= any character other than `"` or `\`

Whitespace within a string literal is significant, and a string literal can span multiple lines, though using such as names seems like a terrible idea.

## Keywords
    
Keywords are used to introduce various definitions and declarations both at top level and within modules, interfaces, and functions. Keywords are syntactically identifiers and must be separated from a subsequent &lt;id-symbol&gt; by &lt;whitespace&gt;. In the syntax descriptions below, keywords are written as literals in a minor abuse of notation.
    
Keywords are reserved identifiers only where necessary to disambiguate the syntax; a record type alias, for example, can also be called `state` even though `state` is a keyword.

## Modules and interfaces
    
Modules are used to collect parameters, constants, and function definitions; interfaces are used to define a particular sort of Arbor functionality, such as ion channel or gap junction dynamics. Module and interface definitions can only be provided at top level.
    
Syntax:

> <module-defn> ::= `module` <identifier> `{` [<parameter-defn> | <constant-defn> | <record-type-alias> | <function-defn> | <module-import> ]* `}`
>
> <parameter-defn> ::= `parameter` [<type-expr>] <identifier> `=` <expression> `;`
>
> <constant-defn> ::= `constant` [<type-expr>] <identifier> `=` <expression> `;`
>
> <record-alias-defn> ::= `record` <identifier> <record-type-body>
>
> <function-defn> ::= `function` <identifier> `(` [ <fn-arg> [`,` <fn-arg>]*  `)` `{` <expression> `}`
>
> <fn-arg> ::= <type-expr> <identifier>
>
> <module-import> ::= `import` <identifier> [ `as` <identifier> ]

Types, records and expressions are described below.

> <interface-defn> ::= `interface` <interface-class> <string-literal> `{` [ [`density`] <pararameter-defn> | <constant-defn> | <record-alias-defn> | <function-defn> | <module-import> | [`density`] <parameter-alias> | <binding> | <initial-decl> | <regime-defn> | <regime-internal-decl>]* `}`
>
> <parameter-alias> ::= [`density`] `parameter` [<type-expr>] <qualified-identifier> [ `as` <identifier> ] `;`
>
> <binding> ::= `bind` [<type-expr>] <identifer> = <bindable-state> `;`
>
> <initial-defn> ::= `initial` [`regime` <qualified-identifier>] [ <initial-post-expr> `from` ] [<type-expr>] `state` `=` <expression> `;`
>
> <initial-post-expr> ::= `steady` | `evolve` `for` <expression>
>
> <regime-defn> ::= `regime` <identifier> `{` [ <regime-internal-defn> | <regime-defn> ] `}`
>
> <regime-internal-defn> ::= `evolvution` `=` <expression> `;` | <when-defn> `;` | <effect-defn> `;`
>
> <when-defn> ::= `when` <when-condition> [ `regime` <qualified-identifier> ] `state` `=` <expression> `;`
>
> <when-condition> ::= <expression> | `event` <identifier> | `post` <identifier>
>
> <effect-defn> ::= <effect> = <expression> `;`
    
The sets of possible bindable states, effects, and interface classes are finite, but may be extended in the future. Not all bindable states or effects are permissible in all interface classes. `density` parameters are intended to describe parameters that may be linearly interpolated, and are only supported in interface classes where this is meaningful.
    
> <interface-class> ::= `density` | `discrete` | `concentration`
>
> <bindable-state> ::= `state` | `membrane` `potential` | `temperature` | (`current` `density` | `molar` `flux`) <species-name>) | (`internal` | `external`) `concentration` <species-name> | `charge` <species-name>
>
> <effect> ::= `current` `density` [<species-name>] | `molar` `flow` `rate` <species-name> | `current` [<species-name>] | `molar` `flux` <species-name> | (`internal` | `external`) `concentration`
>
> <species-name> ::= <string-literal>

### Module semantics
    
TODO: explain parameters; module imports.
    
### Interface semantics

An interface provides, generally:

1. A _state_ that evolves over time, as a function of various local cellular quantities.
2. A set of user-settable parameters.
3. A set of _effects_ that the state imposes locally upon the cell.
    
The initial state and its type is given by the `initial` declaration. For example,
```
    initial state = { a = 0 S; b = 0 S; };
```
declares the state has type `record { conductance a; conductance b; }` and that its initial
value has zero in both fields.

The initial state can be qualified with `steady` or `evolve for` to indicate that the provided value should be integrated over time by some given value or until steady state before being used as an initial value. The evolution is that given by subsequent `evolve` declarations. The degree to which this is supported in an implementation will depend very much on the nature of the evolution function(s).

Evolution is governed by one or more _regimes_. In the simplest case, there is just the one, default regime, but more generally, multiple regimes may be defined, and transitions between them can be triggered by boolean-valued expressions that are a function of state and bound cell quantities, or by pre- or post-synaptic events. Regime definitions can be nested, and an inner regime definition inherits any triggers from the enclosing scope, and the evolution function from the enclosing scope as well, if it is not overridden. The initial regime can be specified in the `initial` definition. The default regime has no name, and so cannot be a target of `regime` clauses in a `when` definition.
    
The evolution function is defined with the `evolution` definition, and there may only be one `evolution` per regime. The expression on the rhs must have type equal to the 'derivative' type of the state. The derivative of a quantity is that quantity divided by time; the derivative of a record type is the record type where each field _x_ is replaced by a field _x'_ with type the derivative type of _x_. See the discusion under record types below for more details.
    
Effects tie the state to the cell, defining how the state determines ionic currents or species flows, or how it governs species concentrations.
    
Parameter definitions are unique to the interface — they are not visible from other interfaces — and can be defined with a default value in the interface definition, or can expose a parameter defined in an imported module.

If the interface doesn't require any evolving state, it can omit the `initial` and `evolution` definitions.
    
#### Density models

Density models represent a process which is distributed over an areal extent of the cell membrane. Parameters defined in density models may be labelled as `density` parameters — such parameters are declared to be safe to interpolate, and can be used with spatitally varying scaling, for example.

Bindable states: state; membrane potential; temperature; internal/external concentration "_species_"; charge "_species_".

Effects: current density (non-specific); current density "_species_"; molar flux "_species_".

Events: only boolean-valued expressions.
    
#### Discrete models
    
Discrete models capture localized effects and can react in response to incoming spike events or post-synaptic spike detection events. They may not have `density` parameters.
    
Bindable states: state; membrane potential; temperature; internal/external concentration "_species_"; charge "_species_".

Effects: current (non-specific); current "_species_"; molar flow rate "_species_".

Events: any.

#### Concentration models

Concentration models determine the internal and/or external concentration of a species over time.
As specified, only one concentration model can be used to determine the state of a species in any
given region of a cell. The initial value of the state determines the initial value(s) of the concentration.
    
Bindable states: state; membrane potential; temperature; current density "_species_"; molar flux "_species_"; charge "_species_". Note that the concentrations themselves are _not_ bindable.

Effects: internal/external concentration "_species_".

Events: only boolean-valued expressions.

#### Alternative concentration model

There are a couple of problems with specifying concentration evolution with concentration models as outlined above:
* Concentration models cannot be combined for any given species.
* Initial concentration is determined by the model, disregarding any user-supplied initial concentration data.

A cleaner approach would be to present a concentration model in terms of the change in concentration over time. Then the initial value would be user-specified, and multiple concentration model contributions could overlap.
    
Example, concentration model:
```
interface concentration "CaDynamics" {
    parameter real gamma = 0.5;
    parameter time decay = 80 ms;
    parameter length detph = 0.1 µm;
    parameter concentration steady_conc = 1.0e-4 mmol/L;
    
    initial state = steady_conc;
    
    bind ca_conc = state;
    bind ca_flux = molar flux "ca";
    evolution = -ca_flux*gamma/depth - (ca_conc-steady_conc)/decay;

    effect internal concentration "ca" = ca_conc;
}
```    

Alternative concentration rate model:
```
interface concentration "CaDynamics" {
    parameter real gamma = 0.5;
    parameter time decay = 80 ms;
    parameter length detph = 0.1 µm;
    parameter concentration steady_conc = 1.0e-4 mmol/L;
    
    bind ca_flux = molar flux "ca";
    bind ca_conc = internal concentration "ca";
    effect internal concentration rate "ca" =
            -ca_flux*gamma/depth - (ca_conc-steady_conc)/decay;
}
```

The alternative model could have been split into two different models, to be applied over the same regions:
```
interface concentration "CaUnbuffered" {
    parameter real gamma = 0.5;
    parameter length depth = 0.1 µm;
    
    bind ca_flux = molar flux "ca";
    effect internal concentration rate "ca" = -ca_flux*gamma/depth;
}

interface concentration "CaBuffered" {
    parameter concentration steady_conc = 1.0e-4 mmol/L;
    parameter time decay = 80 ms;
    
    bind ca_conc = internal concentration "ca";
    effect internal concentration rate "ca" = -(ca_conc-steady_conc)/decay;
}
```

### Function definitions
    
**TODO**
    
## Types and type expressions

Every expression has a type, which is either:
1. A boolean value.
2. A quantity (see below).
3. A record type, comprising an unordered sequence of named fields, with each field being either a quantity or another record type.

Type descriptions are explicitly required in:
1. Record field type declarations.
2. Function parameter declarations.

Type descriptions are optional in contexts where the type can be deduced:
1. In `bind` and `parameter` declarations.
2. In `let` and `with` expressions.
3. In function return value type declarations.

A type expression &lt;type-expr&gt; describes a type; different type expressions may describe the same type.

| <type-expr> ::= `boolean` | <quantity-expr> | <record-type-expr>

### Quantities

Quantities represent physical quantities, which in turn comprise a magnitude and a physical dimension. The specific unit scale underlying the representation of a physical quantity is implicit.

A quantity type is defined as a product term of named quantities such as voltage, time, resistance, etc. The set of named quantities is predefined, and can't be extended within arblang. Quantity syntax:

> <quantity-expr> ::= <quantity-name> | <quantity-product> | <quantity-quotient> | <quantity-power>
>
> <quantity-product> ::= <quantity-expr> <product-symbol> <quantity-expr>
>
> <product-symbol> ::= <whitespace> | <multiplication-dot>
>
> <quantity-quotient> ::= <quantity-term> <division-slash> <quantity-term>
>
> <quantity-power> ::= <quantity> `^` <integer> | <quantity> <integer-superscript>
>
> <integer> ::= [<minus-sign>] <digit>+
> <digit> ::= `0` | `1` | ... | `9`
>
> <integer-superscript> ::= [ `⁻` ] <digit-superscript>+
> <digit-superscript> ::= `⁰` | `¹` | ,,, | `⁹`
>
> <quantity-name> ::= `real` | `length` | `mass` | `time` | `current` | `amount` |
>   `temperature` | `charge` | `frequency` | `voltage` | `resistance` |
>   `capacitance` | `force` | `energy` | `power` | `area` | `volume` | `concentration`

Here, 'real' denotes the dimensionsless (scalar) quantity.

The named quantities above are chosen to represent ISQ quantities, but in some
cases a single word is used to represent a longer ISQ quantity name to simplify
the grammar. These map to ISQ quantities as follows:

`amount`
  ~ amount of substance
    
`current`
  ~ electic current
    
`time`
  ~ time duration
    
`temperature`
  ~ thermodynamic temperature
    
`charge`
  ~ electric charge
    
`resistance`
  ~ electric resistance

`voltage`
  ~ electric potential difference

Examples:

```
let real a = 2.0;
let voltage v = 2 mV;
let current/area/time g' = 1 nA/mm²/ms; # a time derivative of areal current density.
let capacitance⋅length⁻² c = 2 F/cm²; # a contrived way of writing capacitance/area
```

Unlike ISQ quantities, there is one quantity type provided per dimensionally-equivalent class, where 'amount' is considered as its own dimension, distinct from dimensionless quantities. This means, for example, that there is no type support for preventing a concentration of Na⁺ being used in a context expecting a concentration of Ca⁺⁺, or for distinguishing between 'catalytic activity' and 'amount of substance per time', or even between plane and solid angles.

Expressions which evaluate to a given quantity follow normal algebraic rules; constant values must be introduced with a compatible unit.

```
let voltage v = 23 * 10 mV - 2 μV;
```

**Note:** Luminous intensity is excluded above, but perhaps we should include it for completeness.

### Record types

A record is a labelled unordered tuple of values which are either quantities or records themselves. A record may not have two fields of the same name. A record type specification has the syntax:

> <record-type-expr> ::= <record-type> | <record-alias> | <derivative-record>
> <record-alias> ::= <identifier>
> <record-type> ::= `record` <record-type-body>
> <record-type-body> ::= `{` <record-field>* `}`
> <record-field> ::= <type-expr> <identifier> `;`

A record alias is an identifier that has bound to a record type in a record alias definition (see above).
    
#### Record type aliases and derivative records.

See the Modules and Interfaces section for the definition syntax.

When an identifier _x_ is bound as a record alias, then the symbols _x_', _x_'', etc. are also bound as record aliases. The alias _x_' is bound to the _time derivative_ record type of _x_.
    
If a record type has fields named F<sub>1</sub>, F<sub>2</sub>, … with types T<sub>1</sub>, T<sub>2</sub>, … then its time derivative record has fields named F<sub>1</sub>', F<sub>2</sub>', … and types T<sub>1</sub>', T<sub>2</sub>', … where T' is the time derivative record of T if T is a record, or else is the quantity T/time if T is a quantity.
    
Example: with the record aliases
```
    record foo { real a; length b; }
    record bar { amount c; foo d; }
```
the type `bar'` is equivalent to the definition
```
    record bar' {
        amount/time c';
        record { frequency a'; length/time b'; } d';
    }
```

#### Row polymorphism

A record type is a _subrecord_ of another type for every field _F_ in the first record type is also a field _G_ in the second record type with the same field name, and the type of _F_ is either the same as the type of _G_ or is a subrecord of the type of _G_.
    
**TODO:** a record supertype can be bound to a parameter or identifier with a specified record subtype.
    
## Expressions

**TODO:** Expand: an expression can be a comparison expression, an arithmetic expression, a record expression, a function expression, ...

### Literal values

A literal scalar value is just a decimal representation of a real number,
while a non-scalar literal value is written as a decimal number followed by a unit
description. The unit descriptions follow a grammar analagous to the quantity
descriptions (see above).

> <value-literal> ::= <number> | <number> <whitespace> <unit-term>
> <unit-term> ::= <unit> | <unit-product> | <unit-quotient> | <unit-power>
> <unit-product> ::= <unit-term> <multiplication-dot> <unit-term> | <unit-term> <whitespace> <unit-term>
> <unit-quotient> ::= <unit-term> <division-slash> <unit-term>
> <unit-power> ::= <unit-term> `^` <integer> | <unit-term> <integer-superscript>
> <unit> ::= [ <si-prefix-symbol> ] <si-unit-symbol> | <convenience-unit>
> <si-prefix-symbol> ::= `Y` | `Z` | `E` | `P` | `T` | `G` | `M` | `k` | `h` | `da` | `d` | `c` | `m` |
>    `μ` | `µ` | `u` | `n` | `p` | `f` | `a` | `z` | `y`
> <si-unit-symbol> ::= `s` | `m` | `g` | `A` | `K` | `mol` | `L` | `l` | `Hz` | `N` | `J` | `Pa` | `W` | `C` | `F` | `V` | `Ω` | `S` | `kat` | ...
> <convienience-unit> ::= `°C` | `°F` | ...

(Where <number> stands for the regular sorts of decimal representation supported by e.g. JSON.)

Note that `*` is _not_ permitted as a product symbol between units in this proposal. This is in accordance with SI typographical conventions and reduces the risk of misinterpretation where a literal value is used in a term together with an identifier which has the same spelling as an SI unit, e.g.

> 1 cm*cm

would unambiguously mean the product of one centimetre with the value 'cm'. There still remains the problem of e.g.

> 1 cm/cm

which would, if unit terms are given precedence, denote one centimetre per centimetre. As parantheses are also not permitted in unit terms as proposed above, the quotient of one centimetre and the value 'cm' could be denoted by

> 1 cm / cm # Space or lack thereof is significant in unit terms.
> 1 cm/(cm) # Parentheses can't be part of a unit term.

or with maximum clarity, by

> (1 cm)/cm

The type of a literal value is the quantity that is dimensionaly compatible with the given unit.

### Comparison expressions

**TODO**: Things that evaulate to `boolean`.
    
### Arithmetic expressions
    
Arithmetic operations — multiplication, division, addition and subtraction —
can be used to form expressions involving values provided that there is
an agreement in quantity. Exponentiation of scalar values is freely permitted,
but exponentiation of non-scalar values is only defined under particular
circumstances.

**TODO**: All the details.

### Record expressions

A record values can be specified by the field values in a record construction:

> <record-construction> ::= `{` <record-field-binding>* `}`
> <record-field-binding> ::= [ <type-expr> ] <identifier> `=` <field-value> `;`
> <field-value> ::= <expression>

As an example, the following expression has the type `record { voltage a; real b; }`
```
{ real b = 2; a = 3 mV; }
```
where the type of the field `a` is deduced from the rhs of the binding.

Record values support two operations: field access, and substitution.

> <record-expr> ::= <record-value> | `(` <record-expr> `)` | <record-expr> `.` <identifier> | <record-expr> `|` <record-expr>

The update operation `|` takes two record values, with the rhs a subrecord of the lhs,
and returns a record of the same type as the lhs. Each field has the value taken from the rhs
if present in the subrecord type, or otherwise the value taken from the lhs.

In the following example,
```
let p = { a = 3.0; b = 10 A; };
let q = p | { b = 20 A; };
```
the identifier `q` is bound to the record value `{ a = 3.0; b = 20 A; }`.

### Binding expressions

**TODO:** Refactor following for syntax, etc.
    
Let expressions are of the forms:
```
    let _optional type_ _identider_ = _expression1_ _expression2_;
```
or
```
    with _optional type_ _record-valued expression_ _expression2_;
```
The types are optional if they can be deduced from the provided value.
The `with` expression binds an identifier for each field of the record
to the corresponding field value.

The identifiers bound by a `let` or `with` expression have scope
only in `_expression2_`.

TODO: Tidy up/make syntax definition.
