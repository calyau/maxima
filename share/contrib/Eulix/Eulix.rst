.. |b| raw:: html

  &nbsp;

.. |p| raw:: html

  <br/>

==================================================
Eulix - a versatile numerical ODE solver of Maxima
==================================================

----------
Properties
----------

Eulix is an extrapolated, linearly implicit (by default) Euler method.
It can approximate the solution of an initial value problem
for a system of ordinary differential equations or for 
differential algebraic equations (DAEs) of index 1. 
More generally, it can solve an implicit system like  M y'(t)= f(t,y)
where M is a constant mass matrix.

It uses variable step sizes and variable orders.
It offers dense output and root finding, i.e., an implicitly defined
final time. Furthermore, it can deliver an accurate spline approximation
of the solution.

The linearly implicit version is  A-stable up to order 2  and
A(alpha) stable with alpha >= 89.81 up to order 8 and probably above,
where A-stable = A(alpha)-stable with alpha = 90 degrees

`Eulix` is written in `pure Maxima` and all of its coefficients are
rational numbers. Therefore, it can approximate the solution to arbitrary
precision if **fpprec** is set sufficiently high **and** if all floating point
constants in the right-hand-sides and in the initial values are specified as
`bfloat` constants.

-------
Credits
-------

An essential part of `Eulix` consists of a simplified version of the FORTRAN code SEULEX which has been
published in::

  E. Hairer, G.Wanner
  Solving Ordinary Differential Equations II
  Stiff and Differential-Algebraic Problems
  Springer 1991

--------
Features
--------

`Eulix` offers four APIs - one high level one (`Eulix`) which is an extension of the
interface provided by `rk` and `rkf45`,
and two intermediate level interfaces, `Eulix_Table`_ and `Eulix_Spline`_.
Lastly, there is a low-level interface `Eulix_Step`_ which computes a single time step.
|p| All four interfaces allow for a number of options, see the chapter on `Options`_ below.

**Eulix**

`Eulix` is called as::

  Eulix(Expressions,Vars,initials,range,[options])

:Expressions: 
  is a list of expressions (or a single expression)
  which give the r.h.s of the ODE system. |p|
  As a special case there might be
  an equation with r.h.s. or l.h.s equal to 0 to designate an algebraic equation.

:Vars:
  is a list of (dependent) variables which occur in these `expressions`.
  For each variable there has to be an initial value specified by the list `initials`.

:initials: initial values for each of the independent variables.

:range:
  is a list of 3 to 5 elements.

  :``range[1]``:
    specifies the independent variable which will be called `time`
    in the following.


  :``range[2]``:
    and ``range[3]`` specify the initial and final time, respectively. If ``range[3]`` < ``range[2]``
    the ODE is integrated **backwards** in time.


  :``range[4]``:
    is called ``delta_t`` (if present). Depending on an option
    given below, this determines the exact or maximum distance of time values in the
    list of results which `Eulix` returns. If ``delta_t=0`` only the
    natural time steps are returned.


  :``range[5]``:
    is a single  `root condition` or a list of `root conditions` (if present).
    |p| A `root condition` is a real-valued expression which depends on the dependent variables
    (see `Vars`) as well as on the independent variable ``range[1]``.
    The integration stops if either one of the `root conditions` evaluates to zero
    or if the final time ``range[3]`` is reached.

`Eulix` has four different return values.

If there are **no** `root conditions`, `Eulix` returns a list of lists. Each of these
lists consist of a time value and the values of the dependent variables at this
point of time - except if the option ``tabular=none`` (see `Options`_ below).
|p| If the option `combined_t_y_list=false` is given, `Eulix`
returns a list of two lists, the first of which is a list of time values, the second
of which is a list of lists of the values of the dependent variables at the corresponding times.

If there **are** `root conditions`, `Eulix` returns a list of a `solution list`
and what is returned if there are no `root conditions`. 
The `solution list` contains three items: the index of the `root condition` (starting with 1)
which has triggered (i.e. went to zero), the time at which this has happened and
the state (a list of the values of the dependent variables) at this point of time.
|p| If `root conditions` have been specified but none of which has triggered before
the final time ``range[3]`` has been reached, the `solution list` is a list
of three zeros.

In the following we call a matrix with a single column a `vector`.

For the remaining three interfaces you have to specify the right-hand-sides, as well as the root
conditions as vector-valued functiona of two arguments: time and the state vector. Furthermore,
you have to give a vector-valued function which returns the value of the derivative of the 
right-hand-side w.r.t. time. In addition, you have to give a Jacobian matrix of the
right-hand-side; this might be an approximation only. The mass-matrix (even if trivial) has to
specified by the option ``mass_matrix`` (see `Options`_ below).

.. _`Eulix_Table`:

**Eulix_Table**

`Eulix_Table` is called as::

  Eulix_Table(t0,y0,delta_t,t_end,Rhs,Rhs_t,Rhs_jac,Roots,[options])

:t0: initial time

:y0: initial state (a `vector`)

:delta_t: time increment for the generated list of solutions values. Due to `dense output`
  this is independent of the time step used by the integrator. If `delta_t` is zero, only
  the natural time steps or even only the final state are output, depending on the 
  option ``tabular`` (see  `Options`_ below).

:t_end: final time. If ``t_end`` < ``t0`` a negative time step is used. In that case ``delta_t``
  must be negative, as well.

:Rhs: a vector-valued function of (t,y), where y is the state vector. It defines the right-hand-side
  of the ODE system

:Rhs_t: a vector-valued function of (t,y), which gives the partial derivative w.r.t. t

:Rhs_jac: a matrix-valued function of (t,y), which gives the exact or approximate Jacobian
  of the right-hand-side w.r.t. the state vector y

:Roots: a list of scalar-valued functions R(t,y). If R(t,y(t))=0 the integration stops

:options: see `Options`_ below

:Output: if `Roots` is an empty list, `Eulix_Table` returns a list of two lists,
  ``tlist`` and ``ylist`` which contain the points of time and the state vectors at these times, resp.
  - except if the option ``tabular=none``, when only **y_final** is returned.
  |p| if `Roots` is **not** empty, it returns a list **[Solution,[tlist,ylist]]** where
  `Solution` is a list of three values: the index of the `root condition` which triggered the event,
  the time and state of this event. If none of the `root conditions` has triggered, the first element
  of `Solution` is zero - except if the option ``tabular=none`` when only **[Solution,y_final]** 
  is returned


.. _`Eulix_Spline`:

**Eulix_Spline**

`Eulix_Spline` is called as::

  Eulix_Spline(t0,y0,t_end,Rhs,Rhs_t,Rhs_jac,[options])

:parameters: of the same name are the same as those in `Eulix_Table` above.

:options: see `Options`_ below

:Output: `Eulix_Spline` returns a list of objects of struct ``Eulix_Spline_Type``. This is defined as
  |p| ``defstruct(Eulix_Spline_Type(t_left,t_right,NewtonPoly))``
  |p| Each such object defines a polynomial (in ``t``) which represents the solution of the
  ODE / DAE in the time interval ``[t_left,t_right]``. 
  |p| `Eulix` provides a function |b| ``Eulix_SplineEval(t,Spls)`` |b| which evaluates the spline at a point ``t``.
  Here, ``Spls`` is the list returned by `Eulix_Spline`. It's possible to evaluates the
  derivative(s) of this spline w.r.t. time. For this, one should write a function similar to
  `Eulix_SplineEval` - see for example the function `Eulix_Dense_Deriv`, below.


.. _`Eulix_Step`:

**Eulix_Step**

`Eulix_Step` is called as::
  
  [DO_DQ,tn,hn,me,failed]: Eulix_Step(y,t,Rhs,Rhs_t,Rhs_jac,h,me,[options])

:parameters: of the same name are the same as those in `Eulix_Table` above.

:h: step size, which must be non-zero. A negative step size is allowed.
  For the very first step the user must estimate a suitable step size.
  One might use the technique which is applied by `Eulix_Table` - see
  `Automatic selection of the initial step size` in that function.
  For any step except the first one, one should supply the value ``hn``
  as new step size except for the very last step (to hit a desired final time)

:me: width of extrapolation tableau (= order).
  For the very first step the user must estimate a suitable initial order,
  e.g. 6. For any step except the first one, one should supply the value 
  ``me`` which is returned by `Eulix_Step`

:DO_DQ: this value depends on the `dense_output` option, see `Options`_ below.
  If this is false, ``DO_DQ[1]`` contains the new state vector at time ``tn``.
  |p| Otherwiese ``DO_DQ`` contains the dense output information.
  This can be passed to
  |b| ``Eulix_Dense_Out(dt,DO_DQ)`` |b| or |b| ``Eulix_Dense_Deriv(dt,DO_DQ)``.
  |p| Both of these take a time difference ``dt`` relative to the
  **new** time ``tn``. Therefore, to evaluate the solution
  at time ``t1`` in ``[t,tn]`` with ``tn`` > ``t``, the value 
  ``dt`` = ``t1``-``tn`` will be negative. When integrating backwards in time,
  i.e., ``tn`` < ``t``, this value will be positive.
  |p| `Eulix_Dense_Out` evaluates ``y(t)`` while `Eulix_Dense_Deriv` 
  delivers ``y'(t)``. This can be used in Newton's method as is done in
  `Eulix_Table` for determining zeros of the `root conditions`.

:tn: is the new time

:hn: is the new optimal size for the next step. This should be passed 
  as parameter ``h`` to `Eulix_Step` unless this would exceed the desired
  final time.

:me: is the new optimal tableau width for the next step. This should be
  passed unchanged to `Eulix_Step` for the next call.

:failed: If this is `true` the output parameter `DO_DQ` doesn't contain
  useful information. `Eulix_Step` had to reject a step more than 10 times
  in succession. This should only occur if the right-hand-size is
  discontinuous or if the values delivered by `Rhs_t` or `Rhs_jac` are wrong.

.. _`Options`:

**Options**

Some options are only applicable to some of the interface functions above.
The following abbreviations are used to indicate for which function an 
option is effective: `Eul` for `Eulix`, `Tab` for `Eulix_Table`, `Spl` for
`Eulix_Spline` and `Stp` for `Eulix_Step`. 
|p| The options are given in the form `Option Name` = `default`.

:absolute_tolerance: = 1E-6 |b| ``[Eul,Tab,Spl,Stp]``

:relative_tolerance: = 1E-6 |b| ``[Eul,Tab,Spl,Stp]``
  |p| The (local) **required tolerance** is computed as (with the state vector ``y``)::
  
    absolute_tolerance + relative_tolerance *  mat_norm(y,'inf)

:root_tolerance: = 0 |b| This defines the tolerance for Newton's method to
  determine a zero of a `root condition`.
  |p| If this is negative or zero (equivalent to -1E-6), the tolerance is 
  a relative tolerance according to::
  
    abs(root_eps)*max(mat_norm(y_left,'inf),mat_norm(y_right,'inf))
    
  Here, ``y_left`` and ``y_right`` are the state vectors at the left (right, rsp.)
  start interval for Newton's method.

:maximum_order: = 20 |b| ``[Stp]`` |b| This is the maximum tableau width
  that is used by `Eulix_Step`

:dense_output: = true |b| ``[Stp]`` |b| This makes `Eulix_Step` compute the
  dense output information ``DO_DQ``.

:work_estimates: = [1,1,0] |b| ``[Eul,Tab,Spl,Stp]`` |b| For computing
  the optimal step size and order for the next step, `Eulix_Step` needs
  a relative amount of work to evaluate the right-hand-side `Rhs` and
  `Rhs_t`,   to compute the Jacobian `Rhs_jac` and to decompose the Jacobian
  into a `Q-R-decomposition`.

:check_parameters: = true |b| ``[Tab,Spl,Stp]`` |b| If this is true,
  several checks to the parameters are performed. The functions
  ``Eulix_Table`` and ``Eulix_Spline`` set this to false after the first
  call to ``Eulix_Step``.

:mass_matrix: = ``ident(dim)`` |b| This parameter should take a matrix
  of the size of the differential system or ``false``.
  |p| If it has the value ``false``, an extrapolated **explicit** Euler scheme
  is used. In this case, the function `Rhs_jac` will not be called.
  |p| If it has the value of a (square) matrix ``M``, the (possibly)
  implicit ODE |b| ``M y'(t) = f(t,y(t))`` |b| is solved. This matrix need not
  be regular. Indeed, for an algebraic equation, the corressponding row of 
  ``M`` should be zero.
  |p| If `mass_matrix` has the value of a matrix, the **implicit** (extrapolated)
  Euler scheme will be used.

:initial_step_size:  = 0`` |b| ``[Eul,Tab,Spl]`` defines 
  the very first step size; for integration backwards in time, this should
  have a negative value. If it has the (default) value 0, an estimate
  is computed. This depends on the right-hand-side as well as on the option
  ``absolute_tolerance`` - see
  `Automatic selection of the initial step size` in that function `Eulix_Step`.

:initial_order: = 6 |b| ``[Eul,Tab,Spl]`` defines the initial width of the extrapolation
  tableau.

:logging: = ``false`` |b| ``[Tab,Spl,Stp]`` |b| If this is true,
  some internal information is printed, e.g. rejected steps, the initial
  step size, options which are in effect and for each time step the values
  of the current time, step size and tableau width (order).

:tabular: = ``all`` |b| ``[Tab]`` |b| This option can have the values
  ``all`` (default), ``none`` and ``tabular``.
  |p| If it has the value ``none``, only the final state is returned
  or a list **[Solution,y_final]** when `root conditions` have been specified.
  |p| If it has the value ``tabular`` a list of solutions at **equidistant** times
  is produced. For the value ``all``, the natural time steps are intermixed, as well. 

:combined_t_y_list:  = ``true`` |b| ``[Eul]`` |b| If this is true,
  `Eulix` returns a **single list** of lists containing the time value and the
  corresponding solution components at this point of time.
  |p| If this is false, `Eulix` returns |b| ``[tlist,ylist]`` |b|, where
  ``ylist`` is a list of lists containing the solution components at the
  corresponding time in ``tlist``.


--------
Examples
--------

The example files are all named ``Eulix_*.mac``. In the following list, we only
give the suffix which the star stands for.

:T1: This is a simple 1-d ODE which requires frequent changes of the step size
  and order. It's partially stiff. The exact solution is known.

:T1R: This solves the same ODE **backwards** in time

:Step_T1: This solves the same ODE using the low level `Eulix_Step` interface

:T2: This is a simple 1-d ODE which simulates arbitrary stiffness.
  The exact solution is known.

:T3: This is the Volterra-Lotka (2-d) system which is rather demanding.

:T3N: This demonstrates the option ``tabular=none`` for the same ODE

:Spline_T3: This demonstrates the `Eulix_Spline` interface for this ODE

:T4: Here we solve a differential-algebraic equation (DAE) for a pendulum

:T5: This is a rather demanding AIDS model which requires a good error estimator.
  Here we demonstrate the `root finding` feature.

:T5N: Same as above except with ``tabular=none``

:Table_T5: This same model as ``T5`` but this time using the `Eulix_Table`
  interface. Here we demonstrate the `root finding` feature.

:TP: Here we solve a system of 2 equations with a tolerance of 1E-30.
  Since the exact solution is known, we can check the tolerance achieved.

:TS: This is an extremely stiff ODE (`Shampine's ball of flame`)

:Step_TP: A 2-d ODE with a known solution, demonstrating high precision
  solving using the low level `Eulix_Step` interface.

:Table_TC: A very stiff system modelling a chemical reaction using the `Eulix_Table` interface,
  see Hairer/Wanner Solving ODE II .
  
The examples `Eulix_T1.mac`, `Eulix_T2.mac`, `Eulix_T3.mac` and `Eulix_TS.mac`
contain comparisons with `rkf45`.





