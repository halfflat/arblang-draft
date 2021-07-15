# Broad outline

The aim of this proposal is a surface language that is primarily functional,
splitting the definition of a mechanism into a set of free function definitions
and a collection of bindings that describe how the dynamics of a mechanism are
implemented in terms of those functions.

This contrasts with a declarative approach that would define the evolutionary
dynamics of a mechanism in terms of an explicit ODE system (or similar).

## Components of a mechanism

From Arbor's point of view, a mechanism comprises:

*  A 'kind' — the mechanism is either a point or a
   density mechanism. (Leaving aside reversal potential mechanisms for now.)
*  A dynamic state, consisting of zero or more state
   variables which can be inspected by name.
*  A set of named scalar parameters whose values
   can be set at mechanism initialization time, and which are then constant.
*  A set of ion bindings which dictate which values
   of which ions can be read or modified by the mechanism.
*  A closed set of entry points called by Arbor to: initialize mechanism
   state; update state over an integration step; update non-specific and
   ionic currents and conductances; and update ionic concentrations.

NMODL provides (poorly) syntax for all these features, where state update
is described by a system of ODEs or a reaction system over the state variables.
State variables can also be updated directly, outside of an ODE description,
but due to implementation issues, these currently occur as part of the current
update phase.

## State evolution descriptions

In the surface language, we need to be able to describe state evolution
as at least an (explicit) ODE system or reaction system.

NMODL make a distinction between state evolution (via `DERIVATIVE` blocks)
and setting initial state. This distinction is useful for the case where
the initialization may be non-trivial: e.g. when the initial state should
correspond to a steady-state solution of the ODE system.

As noted above, NMODL quite happily mixes a description of non-integrative
state updates with current updates etc., which is not ideal. An approach
that capture both in a matematically neat way is to define the evolution
in terms of a state machine: each state is governed by an ODE description,
and transitions between states are described by threshold criteria or
by domain definitions, together with a transfromation of the state
vector.

We should also support, at least partially, dynamics described by a system
of stochastic differential equationds. For one-dimensional (time) processes,
these can typically be described in the form
> d_X_(_t_) = _f_(_X_,_t_) + _g_(_X_,_t_)d_W_(_t_)
where _W_ is the Wiener process, but with some abuse of notation, this
can be written like an ODE
> _X_' = _f_(_X_,_t_) = _g_(_X_,_t_)ζ(_t_)
where ζ represents (n-dimensional) Gaussian white noise. A symbolic
representation could then piggyback on a symbolic ODE representation with
a new special symbol representing ζ; a functional representaion as
proposed below would use an SDE binding that specified the evolution in
terms of the functions _f_ and _g_.

Other possible evolutions that we might want to include in the future include
stochastic partial differential equation systems, which include a noise term
with spatial correlations, and stochastic reaction systems that represent (part
of) the state in terms of species counts rather than continuous quantities, and
their evolution is govverned by a continuous time Markov process described in
turn by a system of reaction equations.

In the current state of Arbor, an ODE solver is determined automatically by
symbolic analysis of the provided ODE system. This can be performed also for
arblang, with or without explicit hints. Such hints fit more naturally where
the state evolution description is 'bound' to a mechanism interface in the
language, rather than as part of the ODE description itself.

## Ionic bindings

A mechanism can have ion dependencies, in that the state evolution can be
modulated by ionic concentrations, and that the current contributions can be
tied to specific ionic flows. More generally, a mechanism could be used to
describe the flux of a species across the mebrane, even if that species carries
no charge, though this cannot be specified in a natural way in NMODL.

While biophysical models are generally specified in terms of specific ionic
species, it can be useful to maintain multiple, distinct ionic populations of
the same species, and a parameterized mechanism might be repurposed to
represent the dynamics of a different ion without any changes to its
mathematical description. How should this sort of flexibility be represented in
an arblang mechanism description?

A couple of proposals are presented below in terms of the binding interface.

## Ion concentration evolution

NMODL makes no real distinction between mechanisms that describe ion channel
process and mechansisms that describe the evolution of ionic concentrations.

As the initial state of a mechanism can depend upon correct ion concentrations,
and as those concentrations may be set by a different mechanism governing ionic
concentrations, there arises a dependency problem. Additionally, what should it
mean if multiple mechanisms are responsible for maintaining the concentration
of the same ionic species?

A possible solution is to describe such processes by (literally) a different
class of mechanism; concentration mechanisms would have a different set of
bindings. Two approaches come to mind:

1.  An ion in any given region is governed by at most one such concentration
    mechanism. The mechanism can provide initial ion concentrations and a state
    evolution for a state which includes the relevant concentrations and which
    takes total ionic currents or fluxes as parameters.

2.  A concentration model provides only fluxes: it does not set the initial
    ionic concentration — this is supplied externally — and its state
    evolution, if any, does not include any explicit ion concentrations. Ionic
    updates are specified in terms of rates — instantaneous change in
    concentration per time. These could then be safely accumulated over
    multiple mechanisms, and a change in concentration would consitute a
    time integral over these rates. This would also be compatible with any
    diffusive processes we would want to incorporate in the future.

