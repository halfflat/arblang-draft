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

&lt;reaction-right&gt;, &lt;result-arrow&gt;
  ~ U+2192 `→` RIGHTWARDS ARROW or the sequence `->`.

&lt;reaction-left&gt;
  ~ U+2190 `←` LEFTWARDS ARROW or the sequence `<-`.

&lt;reaction-rightleft&gt;
  ~ U+21C4 `⇄` RIGHT ARROW OVER LEFT ARROW or the sequence `<->`.

Allowing multiple symbols in these instances allows flexibility in input environments and easier copy/paste from mathematical software. A goal is to allow mathematical and quantity expressions to be represented similarly to how they might be typeset.

## Whitespace

Outside of string literals (see below), all non-empty sequences of whitespace are treated equivalently.

> &lt;whitespace&gt; ::= &lt;whitespace-char&gt;+
>
> &lt;whitespace-char&gt; ::= any character with the Unicode property `White_Space`.

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

> &lt;identifier&gt; ::= &lt;initial-id-symbol&gt; [&lt;id-symbol&gt;]*
>
> &lt;id-symbol&gt; ::= &lt;initial-id-symbol&gt; | &lt;other-id-symbol&gt;

Identifiers broadly follow the conventions of Python, where below, braces are used to denote Unicode character classes or properties.

> &lt;initial-id-symbol&gt; ::= {L} | {Nl} | {Other_ID_Start}
>
> &lt;other-id-symbol&gt; ::= {Nd} | {Mn} | {Mc} | {Pc} | &lt;prime-mark&gt;
>
> &lt;prime-mark&gt; ::= `'` | `ʹ` U+02B9 MODIFIER LETTER PRIME | `′` U+2032 PRIME

&lt;initial-id-symbol&gt; can be any character in the Letter 'L' category, in Number-letter 'Nl' category, or any character with the 'Other_ID_Start' property, which corresponds to the permitted Python starting characters with the exception of the underscore.

&lt;other-id-symbol&gt; includes digits, underscores and similar punctuation, but also &lt;prime-mark&gt;, which is used to denote a derivative.

Unlike Python, characters that are only _compatibility_ equivalent to acceptable identifier symbols are not permitted. In particular, superscript numerals are not permitted within an identifier.

Two identifiers are equivalent if they have the same canonical representation, which corresponds to the Unicode NFKC normalization with the additional mapping of any character in &lt;prime-mark&gt; to `'`.

### Qualified identifier

A qualified identifier is a particular sort of expression that identifies a definition in an imported module, or a sub-regime within a regime.

> &lt;qualified-identifier&gt; ::= &lt;identifier&gt; | &lt;qualified-identifier&gt; `.` &lt;identifier&gt;

## Names

Some objects can take arbitrary labels rather than identifiers, namely chemical species and interface names. These are represented by string literals:

> &lt;string-literal&gt; ::= `"` [ `\"` | `\\` | &lt;unescaped-char&gt; ]* `"`
>
> &lt;unescaped-char&gt; ::= any character other than `"` or `\`

Whitespace within a string literal is significant, and a string literal can span multiple lines, though using such as names seems like a terrible idea.

## Keywords

Keywords are used to introduce various definitions and declarations both at top level and within modules, interfaces, and functions. Keywords are syntactically identifiers and must be separated from a subsequent &lt;id-symbol&gt; by &lt;whitespace&gt;. In the syntax descriptions below, keywords are written as literals in a minor abuse of notation.

Keywords are reserved identifiers only where necessary to disambiguate the syntax; a record type alias, for example, can also be called `state` even though `state` is a keyword.

## Modules and interfaces

Modules are used to collect parameters, constants, and function definitions; interfaces are used to define a particular sort of Arbor functionality, such as ion channel or gap junction dynamics. Module and interface definitions can only be provided at top level.

Syntax:

> &lt;module-defn&gt; ::= `module` &lt;identifier&gt; `{` [&lt;parameter-defn&gt; | &lt;constant-defn&gt; | &lt;record-type-alias&gt; | &lt;function-defn&gt; | &lt;module-import&gt; ]* `}`
>
> &lt;parameter-defn&gt; ::= `parameter` [&lt;type-expr&gt;] &lt;identifier&gt; `=` &lt;expression&gt; `;`
>
> &lt;constant-defn&gt; ::= `constant` [&lt;type-expr&gt;] &lt;identifier&gt; `=` &lt;expression&gt; `;`
>
> &lt;record-alias-defn&gt; ::= `record` &lt;identifier&gt; &lt;record-type-body&gt;
>
> &lt;function-defn&gt; ::= `function` &lt;identifier&gt; `(` [ &lt;fn-arg&gt; [`,` &lt;fn-arg&gt;]*  `)` [ &lt;result-arrow&gt; &lt;type-expr&gt; ] `{` &lt;expression&gt; `}`
>
> &lt;fn-arg&gt; ::= &lt;type-expr&gt; &lt;identifier&gt;
>
> &lt;module-import&gt; ::= `import` &lt;identifier&gt; [ `as` &lt;identifier&gt; ]

Types, records and expressions are described below.

> &lt;interface-defn&gt; ::= `interface` &lt;interface-class&gt; &lt;string-literal&gt; `{` [ &lt;pararameter-defn&gt; | &lt;export&gt; | |&lt;combined-export-parameter-defn&gt; | &lt;constant-defn&gt; | &lt;record-alias-defn&gt; | &lt;function-defn&gt; | &lt;module-import&gt; | &lt;binding&gt; | &lt;initial-decl&gt; | &lt;regime-defn&gt; | &lt;regime-internal-decl&gt;]* `}`
>
> &lt;export&gt; ::= `export` [`density`] `parameter` [&lt;type-expr&gt;] &lt;qualified-identifier&gt; [ `as` &lt;identifier&gt; ]
>
> &lt;combined-export-parameter-defn&gt; ::= `export` [`density`] <parameter-defn>
>
> &lt;binding&gt; ::= `bind` [&lt;type-expr&gt;] &lt;identifer&gt; = &lt;bindable-state&gt; `;`
>
> &lt;initial-defn&gt; ::= `initial` [`regime` &lt;qualified-identifier&gt;] [ &lt;initial-post-expr&gt; `from` ] [&lt;type-expr&gt;] `state` `=` &lt;expression&gt; `;`
>
> &lt;initial-post-expr&gt; ::= `steady` | `evolve` `for` &lt;expression&gt;
>
> &lt;regime-defn&gt; ::= `regime` &lt;identifier&gt; `{` [ &lt;regime-internal-defn&gt; | &lt;regime-defn&gt; ] `}`
>
> &lt;regime-internal-defn&gt; ::= `evolve` [`explicit`] [&lt;type-expr&gt;] `state'` `=` &lt;expression&gt; `;` | &lt;when-defn&gt; `;` | &lt;effect-defn&gt; `;`
>
> &lt;when-defn&gt; ::= `when` &lt;when-condition&gt; `;` [ `regime` &lt;qualified-identifier&gt; ] `state` `=` &lt;expression&gt; `;`
>
> &lt;when-condition&gt; ::= &lt;expression&gt; | [&lt;type-expr&gt;] &lt;identifier&gt = ( `event` | `post` ) 
>
> &lt;effect-defn&gt; ::= &lt;effect&gt; = &lt;expression&gt; `;`

The sets of possible bindable states, effects, and interface classes are finite, but may be extended in the future. Not all bindable states or effects are permissible in all interface classes. `density` parameters are intended to describe parameters that may be linearly interpolated, and are only supported in interface classes where this is meaningful.

> &lt;interface-class&gt; ::= `density` | `discrete` | `concentration`
>
> &lt;bindable-state&gt; ::= `state` | `membrane` `potential` | `temperature` | (`current` `density` | `molar` `flux`) &lt;species-name&gt;) | (`internal` | `external`) `concentration` &lt;species-name&gt; | `charge` &lt;species-name&gt;
>
> &lt;effect&gt; ::= `current` `density` [&lt;species-name&gt;] | `molar` `flow` `rate` &lt;species-name&gt; | `current` [&lt;species-name&gt;] | `molar` `flux` &lt;species-name&gt; | (`internal` | `external`) `concentration`
>
> &lt;species-name&gt; ::= &lt;string-literal&gt;

### Module semantics

**TODO:** explain parameters; module imports.

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

The evolution function is defined with the `evolve` definition, and there may only be one `evolve` per regime. The expression on the rhs must have type equal to the 'derivative' type of the state. The derivative of a quantity is that quantity divided by time; the derivative of a record type is the record type where each field _x_ is replaced by a field _x'_ with type the derivative type of _x_. See the discusion under record types below for more details.

Effects tie the state to the cell, defining how the state determines ionic currents or species flows, or how it governs species concentrations.

Exports make parameters visible and settable by models that employ the interface. As a convenience, an export and a parameter definition can be combined in a single export declaration.

If the interface doesn't require any evolving state, it can omit the `initial` and `evolve` definitions.

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

Concentration models determine the internal and/or external concentration of a species over time. As specified, only one concentration model can be used to determine the state of a species in any given region of a cell. The initial value of the state determines the initial value(s) of the concentration.

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
    export parameter real gamma = 0.5;
    export parameter time decay = 80 ms;
    export parameter length detph = 0.1 µm;
    export parameter concentration steady_conc = 1.0e-4 mmol/L;

    initial state = steady_conc;

    bind ca_conc = state;
    bind ca_flux = molar flux "ca";
    evolve state' = -ca_flux*gamma/depth - (ca_conc-steady_conc)/decay;

    effect internal concentration "ca" = ca_conc;
}
```

Alternative concentration rate model:
```
interface concentration "CaDynamics" {
    export parameter real gamma = 0.5;
    export parameter time decay = 80 ms;
    export parameter length detph = 0.1 µm;
    export parameter concentration steady_conc = 1.0e-4 mmol/L;

    bind ca_flux = molar flux "ca";
    bind ca_conc = internal concentration "ca";
    effect internal concentration rate "ca" =
            -ca_flux*gamma/depth - (ca_conc-steady_conc)/decay;
}
```

The alternative model could have been split into two different models, to be applied over the same regions:
```
interface concentration "CaUnbuffered" {
    export parameter real gamma = 0.5;
    export parameter length depth = 0.1 µm;

    bind ca_flux = molar flux "ca";
    effect internal concentration rate "ca" = -ca_flux*gamma/depth;
}

interface concentration "CaBuffered" {
    export parameter concentration steady_conc = 1.0e-4 mmol/L;
    export parameter time decay = 80 ms;

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

| &lt;type-expr&gt; ::= `boolean` | &lt;quantity-expr&gt; | &lt;record-type-expr&gt;

### Quantities

Quantities represent physical quantities, which in turn comprise a magnitude and a physical dimension. The specific unit scale underlying the representation of a physical quantity is implicit.

A quantity type is defined as a product term of named quantities such as voltage, time, resistance, etc. The set of named quantities is predefined, and can't be extended within arblang. Quantity syntax:

> &lt;quantity-expr&gt; ::= &lt;quantity-name&gt; | &lt;quantity-product&gt; | &lt;quantity-quotient&gt; | &lt;quantity-power&gt;
>
> &lt;quantity-product&gt; ::= &lt;quantity-expr&gt; &lt;product-symbol&gt; &lt;quantity-expr&gt;
>
> &lt;product-symbol&gt; ::= &lt;whitespace&gt; | &lt;multiplication-dot&gt;
>
> &lt;quantity-quotient&gt; ::= &lt;quantity-term&gt; &lt;division-slash&gt; &lt;quantity-term&gt;
>
> &lt;quantity-power&gt; ::= &lt;quantity&gt; `^` &lt;integer&gt; | &lt;quantity&gt; &lt;integer-superscript&gt;
>
> &lt;integer&gt; ::= [&lt;minus-sign&gt;] &lt;digit&gt;+
>
> &lt;digit&gt; ::= `0` | `1` | ... | `9`
>
> &lt;integer-superscript&gt; ::= [ `⁻` ] &lt;digit-superscript&gt;+
>
> &lt;digit-superscript&gt; ::= `⁰` | `¹` | ,,, | `⁹`
>
> &lt;quantity-name&gt; ::= `real` | `length` | `mass` | `time` | `current` | `amount` |
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

> &lt;record-type-expr&gt; ::= &lt;record-type&gt; | &lt;record-alias&gt; | &lt;derivative-record&gt;
>
> &lt;record-alias&gt; ::= &lt;identifier&gt;
>
> &lt;record-type&gt; ::= `record` &lt;record-type-body&gt;
>
> &lt;record-type-body&gt; ::= `{` &lt;record-field&gt;* `}`
>
> &lt;record-field&gt; ::= &lt;type-expr&gt; &lt;identifier&gt; `;`

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

An expression corresponds to something that can be evaluated to give a value. They can be a literal value, or some
combination of literal values, bound identifiers, arithemetic and record operations, function invocations, local bindings, and comparisons.

> &lt;expression&gt; = &lt;value-literal&gt; | `(` &lt;expression;&gt; `)` | &lt;function-call&gt; | &lt;arithemtic-expr&gt; | &lt;record-expr&gt; | &lt;boolean-expr&gt; | &lt;conditional-expr&gt; | &lt;let-expr&gt; | &lt;with-expr&gt;

### Literal values

A literal scalar value is just a decimal representation of a real number,
while a non-scalar literal value is written as a decimal number followed by a unit
description. The unit descriptions follow a grammar analagous to the quantity
descriptions (see above).

> &lt;value-literal&gt; ::= &lt;number&gt; | &lt;number&gt; &lt;whitespace&gt; &lt;unit-term&gt;
>
> &lt;unit-term&gt; ::= &lt;unit&gt; | &lt;unit-product&gt; | &lt;unit-quotient&gt; | &lt;unit-power&gt;
>
> &lt;unit-product&gt; ::= &lt;unit-term&gt; &lt;multiplication-dot&gt; &lt;unit-term&gt; | &lt;unit-term&gt; &lt;whitespace&gt; &lt;unit-term&gt;
>
> &lt;unit-quotient&gt; ::= &lt;unit-term&gt; &lt;division-slash&gt; &lt;unit-term&gt;
>
> &lt;unit-power&gt; ::= &lt;unit-term&gt; `^` &lt;integer&gt; | &lt;unit-term&gt; &lt;integer-superscript&gt;
>
> &lt;unit&gt; ::= [ &lt;si-prefix-symbol&gt; ] &lt;si-unit-symbol&gt; | &lt;convenience-unit&gt;
>
> &lt;si-prefix-symbol&gt; ::= `Y` | `Z` | `E` | `P` | `T` | `G` | `M` | `k` | `h` | `da` | `d` | `c` | `m` |
>    `μ` | `µ` | `u` | `n` | `p` | `f` | `a` | `z` | `y`
>
> &lt;si-unit-symbol&gt; ::= `s` | `m` | `g` | `A` | `K` | `mol` | `L` | `l` | `Hz` | `N` | `J` | `Pa` | `W` | `C` | `F` | `V` | `Ω` | `S` | `kat` | ...
>
> &lt;convienience-unit&gt; ::= `°C` | `°F` | ...

(Where &lt;number&gt; stands for the regular sorts of decimal representation supported by e.g. JSON.)

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

### Boolean expressions and conditionals

Boolean expressions are those expressions that evalue to a boolean value. They comprise the boolean literals `true` and `false`, and comparisons.

> &lt;boolean-expr&gt; ::= `true` | `false` | &lt;expression&gt; &lt;comparison-op&gt; &lt;expression&gt; | &lt;expression&gt; &lt;logical-op&gt; &lt;expression&gt;
>
> &lt;comparison-op&gt; ::= `<` | `<=` | `>` | `>=` | `==` | `!=`
>
> &lt;logical-op&gt; ::= `not` | `and` | `or`

Conditional expressions evaluate to one of two sub-expressions based on a boolean value.

> &lt;conditional-expr&gt; ::= `if` &lt;expression&gt; `;` `then` &lt;expression&gt; `else` &lt;expression&gt;

#### Alternative conditional syntax

In order to avoid nested `if` expressions, we could also or instead adopt a guard-style syntax. The last clause must have condition `true`, or as syntactic sugar, `otherwise`.

**TODO:** Syntax definition.

Example:
```
let q =
  | a==3 => 1
  | otherwise => sin(a)/a;
```

#### Semantics

Comparisons can only be performed between quality values of the same type, or between records that are of the same type, or where one is a subrecord of the other. For record comparisons, the expression evaluates to true if the comparison holds true for each field in the subrecord.

Logical operations are only defined for boolean values.

### Arithmetic expressions

Arithmetic operations — multiplication, division, addition and subtraction —
can be used to form expressions involving values provided that there is
an agreement in quantity. Exponentiation of scalar values is freely permitted,
but exponentiation of non-scalar values is only defined under particular
circumstances.

**TODO**: All the details.

### Record expressions

A record values can be specified by the field values in a record construction:

> &lt;record-construction&gt; ::= `{` &lt;record-field-binding&gt;* `}`
>
> &lt;record-field-binding&gt; ::= [ &lt;type-expr&gt; ] &lt;identifier&gt; `=` &lt;field-value&gt; `;`
>
> &lt;field-value&gt; ::= &lt;expression&gt;

As an example, the following expression has the type `record { voltage a; real b; }`
```
{ real b = 2; a = 3 mV; }
```
where the type of the field `a` is deduced from the rhs of the binding.

Record values support two operations: field access, and update.

> &lt;record-expr&gt; ::= &lt;field-expr&gt; | &lt;update-expr&gt;
>
> &lt;field-expr&gt; ::= &lt;expression&gt; `.` &lt;identifier&gt;
>
> &lt;update-expr&gt; ::= &lt;expression&gt; `:` &lt;expression&gt;

The update operation `:` takes two record values, with the rhs a subrecord of the lhs, and returns a record of the same type as the lhs. Each field has the value taken from the rhs if present in the subrecord type, or otherwise the value taken from the lhs.

In the following example,
```
let p = { a = 3.0; b = 10 A; };
let q = p : { b = 20 A; };
```
the identifier `q` is bound to the record value `{ a = 3.0; b = 20 A; }`.

### Local binding expressions

`let` and `with` bind identifiers to expressions or record fields. `with` could also possibly be used to bring into scope identifiers defined in an imported module.

> &lt;let-expr&gt; ::= `let` [&lt;type-expr&gt;] &lt;identifier&gt; = &lt;expression&gt; `;` &lt;expression&gt;
> &lt;with-expr&gt; ::= `with` [&lt;type-expr&gt;] &lt;expression&gt; `;` &lt;expression&gt;

The identifiers introduced by `let` or `with` have only the terminal expression as scope.

**TODO:** Examples!

## Syntax discussion and alternatives

### Semicolons

The proposal above requires semicolons only in three circumstances: when associating or binding something after an equals sign; when making local bindings in a with-expression; after a conditional in a when-clause; or when declaring fields in a record type:
```
# Semicolons after `=` and expression:
parameter length² A = 3 m²;
bind S = state;
effect current "na" = 3 mA;
let real a = S.a; 3+a

# Semicolon after first expression in `with`:
with S; 3+a

# Semicolon after field definition in record type,
# as well as in record value fields.
let record { concentration x; } b = { x = 3; }; 3+b.x

# No semicolon after record type alias.
record foo { real x; }

# Or after function definition.
function bar () { 3 }

# Or after a paramater alias,
density parameter ca2_dynamics.foo as foo

# OR after import, etc.
import std as foobar

```

Why have semicolons at all? It can disambiguate alternative possible parses when two expressions are adjacent, which is a situation that arises from allowing whitespace to act as a product in quantity and unit expressions:
```
let A = 3 A A - 4 A
```
could be interpreted as binding `A` to the value 3 square amperes and returning negative four amperes, or binding `A` to the value 3 amperes, and then returning `A` minus four amperes, giving negative one ampere. The semicolon resolves this one way or the other:
```
let A = 3 A; A - 4 A
```

Why not have semicolons more commonly? It's certainly possible: they could be added after every `import` or record alias or function definition, but it is not necessary for the resolution of ambiguities. An alternate function and alias syntax described below could help unify some of these syntaxes.


### Function alternatives and type aliases

As presented above, functions are fairly inflexible: they can only be defined in module/interface scope, and they are not permitted to be polymorphic.

Functions could be allowed to be defined locally with a syntax like e.g.
```
function five() {
    let f = function (real a, real b) { a+b };
    f(3, 2)
}
```
in which case a uniform definition syntax might replace `function _name_ (params...) ...`, e.g.
```
def five = function () {
    let f = function (real a, real b) { a+b };
    f(3, 2)
};
```
And rather than be so verbose, we could use something like `fn` instead.

A problem with this is that it makes functions look like values, and values should be things you can return from functions, etc., and suddenly for consistency it looks like we might want to support functions as first class objects. This would increase expressiveness, but would also make the type system more complicated. For the domain, this is probably not an expressiveness we need.

However, we could make a distinction between binding local identifiers with `let` and introducing new function or record aliases with e.g. `def`. Then the example would become
```
def five = fn () {
    def f = fn (real a, real b) { a+b };   # 'f' is _not_ a value!
    f(3, 2)
};
```
and this would allow a general approach to function aliases, type naming, and derivative type forms:
```
def model_state = record { real a; concentration b; }; # record alias

def ca_model = concentration; # quantity alias
def ca_rate = fn (ca_model conc) -> ca_model' { conc/20 ms }; # ca_model' automatically defined as concentration/time

import std
def nernst = std.nernst; # 'nernst' is another name for the 'std.nernst' function.
```

If `def foo = fn (...) { ... };` looks a little verbose, we could keep `def foo(...) { ... }` as syntactic sugar.

### Magic keywords and extension points

Within an interface block, there are specific points within the permitted syntax where keywords are used to refer to interface-specific values or concepts, and which constitute natural places for future extensions of the interface block to support new functionality:

1. Right hand side of `bind`, corresponding to interface-specific exposed values, such as the mechanism state, ion concentrations, membrane voltage, etc.
2. Right hand side of local binding in a `when` clause, tying event or post-event data to an identifier.
3. Left hand side of `effect`, corresponding to contributions of the interface to the simulation state.
4. Left hand side of `initial`, where the optional `... from` clause modifies the initial state, and also where `state` forms the left hand side of the final binding.
5. Left hand side of `evolve`, where the keyword `explicit` might be replaced with other possible descriptions, such as `implicit` for implicit ODE or DAE systems, and also where `state'` forms left hand side of the final binding.
6. Left hand side of `export`, where we have `density parameter` or `parameter`, but which could be extended to support for example the exposure of derived values to probe requests or similar.

**TODO:** Add examples of possible syntax extensions in these cases.
