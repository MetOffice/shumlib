API Reference: shum_horizontal_field_interp
-------------------------------------------

Fortran Functions/Subroutines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

``get_shum_horizontal_field_interp_version``
''''''''''''''''''''''''''''''''''''''''''''

All Shumlib libraries expose a module and function named in this format; it 
allows access to the Shumlib version number used when compiling the library.

    **Available via module**
        ``f_shum_horizontal_field_interp_version_mod``

    **Syntax**
        ``version = get_shum_horizontal_field_interp_version()``

    **Returns**
        ``version (INTEGER)``
            Shumlib version number, in format ``YYYYMMX`` (the 4-digit year
            followed by 2-digit month and an additional digit signifying the
            release count within that specified month).

``f_shum_horizontal_field_bi_lin_interp_get_coeffs``
''''''''''''''''''''''''''''''''''''''''''''''''''''

This routine calculates indices of the bottom left and right corners in
the source grid used to calculate the target grid value and the weights
of the 4 surrounding source grid points needed to calculate the target value.

    **Available via module**
        ``f_shum_horizontal_field_interp_mod``

    **Syntax**
        ``f_shum_horizontal_field_bi_lin_interp_get_coeffs ( index_b_l, index_b_r, weight_t_r, weight_b_r, weight_t_l, weight_b_l, lambda_srce, phi_srce, lambda_targ, phi_targ, points_lambda_srce, points_phi_srce, points, cyclic )``

    **Inputs**
        ``points_lambda_srce (32-bit INTEGER)``
            Number of lambda points on source grid
        ``points_phi_srce (64-bit INTEGER)``
            Number of phi points on source grid
        ``points (64-bit INTEGER)``
            Total number of points on target grid
        ``lambda_targ(points) (64-bit REAL)``
            Lambda coords of target grid in degrees using same rotation as source grid
        ``phi_targ(points) (64-bit REAL)``
            Phi coords of target grid in degrees using same rotation as source grid
        ``lambda_srce(points_lambda_srce) (64-bit REAL)``
            Lambda coords of source grid in degrees
        ``phi_srce(points_phi_srce) (64-bit REAL)``
            Phi coords of source grid in degrees
        ``cyclic (64-bit LOGICAL)``
            =T, then source data is cyclic
            =F, then source data is non-cyclic
    
    **Outputs**
        ``index_b_l(points) (64-bit INTEGER)``
            Index of bottom left corner of source gridbox
        ``index_b_r(points) (64-bit INTEGER)``
            Index of bottom right corner of source gridbox
        ``weight_t_r(points) (64-bit REAL)``
            Weight applied to value at top right corner of source gridbox
        ``weight_b_l(points) (64-bit REAL)``
            Weight applied to value at bottom left corner of source gridbox
        ``weight_b_r(points) (64-bit REAL)``
            Weight applied to value at bottom right corner of source gridbox
        ``weight_t_l(points) (64-bit REAL)``
            Weight applied to value at top left corner of source gridbox

    **Notes**
        ALL arguments here are 64-bit.


``f_shum_horizontal_field_bi_lin_interp_calc``
''''''''''''''''''''''''''''''''''''''''''''''

This routine takes the input indices and weights and calculates the value
of the output data points.

    **Available via module**
        ``f_shum_horizontal_field_interp_mod``

    **Syntax**
        ``f_shum_horizontal_field_bi_lin_interp_calc( rows_in, row_length_in, len_field_out, index_b_l, index_b_r, data_in, weight_b_l, weight_b_r, weight_t_l, weight_t_r, data_out )``

    **Inputs**
        ``rows_in (64-bit INTEGER)``
            Number of P rows on source grid
        ``row_length_in (64-bit INTEGER)``
            Number of pts per row on source grid
        ``len_field_out (64-bit INTEGER)``
            Number of points on target grid
        ``index_b_l(len_field_out) (64-bit INTEGER)``
            Index of bottom left corner of source gridbox
        ``index_b_r(len_field_out) (64-bit INTEGER)``
            Index of bottom righ corner of source gridbox
        ``data_in(rows_in*row_length_in) (64-bit REAL)``
            Data before interpolation
        ``weight_b_l(len_field_out) (64-bit REAL)``
            Weight applied to value at bottom left corner of source gridbox
        ``weight_b_r(len_field_out) (64-bit REAL)``
            Weight applied to value at bottom right corner of source gridbox
        ``weight_t_l(len_field_out (64-bit REAL)``
            Weight applied to value at top left corner of source gridbox
        ``weight_t_r(len_field_out) (64-bit REAL)``
            Weight applied to value at top right corner of source gridbox

    **Outputs**
        ``data_out(len_field_out) (64-bit REAL)``
            Data after interpolation

    **Notes**
        The arguments here are *all* 64-bit.


``f_shum_find_source_box_indices``
''''''''''''''''''''''''''''''''''

This routine locates the lat/long indices of the points in the target grid
and the indecies of the points in the source grid that form the bottom left
and right corners of the surrounding grid box.
This routine is used by f_shum_horizontal_field_bi_lin_interp_get_coeffs
and is not intended for direct use.

    **Available via module**
        ``f_shum_horizontal_field_interp_mod``

    **Syntax**
        ``f_shum_find_source_box_indices( index_b_l, index_b_r, lambda_srce, phi_srce, lambda_targ, phi_targ, points_lambda_srce, points_phi_srce, points, cyclic, t_lambda, ixp1, ix, iy )``

    **Inputs**
        ``points_lambda_srce (64-bit INTEGER)``
            Number of lambda points on source grid
        ``points_phi_srce (64-bit INTEGER)``
            Number of phi points on source grid
        ``points (64-bit INTEGER)``
            Total number of points on target grid
        ``lambda_targ(points) (64-bit REAL)``
            Lambda coords of target grid in degree using same rotation
            as source grid
        ``phi_targ(points) (64-bit REAL)``
            Phi coords of target grid in degrees using same rotation
            as source grid
        ``lambda_srce(points_lambda_srce) (64-bit REAL)``
            Lambda coords of source grid in degrees
        ``phi_srce(points_phi_srce) (64-bit REAL)``
            Phi coords of source grid in degrees
        ``cyclic (64-bit LOGICAL)``
            =T, then source data is cyclic
            =F, then source data is non-cyclic

    **Outputs**
        ``index_b_l(points) (64-bit INTEGER)``
            Index of bottom left corner of source gridbox
        ``index_b_r(points) (64-bit INTEGER)``
            Index of bottom right corner of source gridbox
        ``ixp1(points) (64-bit INTEGER)``
            Longitudinal index plus 1
        ``ix(points) (64-bit INTEGER)``
            Longitudinal index
        ``iy(points) (64-bit INTEGER)``
            Latitudinal index
        ``t_lambda(points) (64-bit REAL)``
            Local value of target longitude

    **Notes**
        The arguments here are *all* 64-bit.

``f_shum_calc_weights``
'''''''''''''''''''''''

This routine calculates the weights required for bi-linear interpolation of
the values on the 4 source grid points which surround the target grid point.
This routine is used by f_shum_horizontal_field_bi_lin_interp_get_coeffs
and is not intended for direct use.

    **Available via module**
        ``f_shum_horizontal_field_interp_mod``

    **Syntax**
        ``f_shum_wgdos_unpack( weight_t_r, weight_b_r, weight_t_l, weight_b_l, lambda_srce, phi_srce, phi_targ, points_lambda_srce, points_phi_srce, points, t_lambda, ixp1, ix, iy )``

    **Inputs**
        ``points_lambda_srce (64-bit INTEGER)``
            Number of lambda points on source grid
        ``points_phi_srce (64-bit INTEGER)``
            Number of phi points on source grid
        ``points (64-bit INTEGER)``
            Total number of points on target grid
        ``ixp1(points (64-bit INTEGER)``
            Longitudinal index plus 
        ``ix(points) (64-bit INTEGER)``
            Longitudinal index
        ``iy(points) (64-bit INTEGER)``
            Latitudinal index
        ``phi_targ(points) (64-bit REAL)``
            Phi coords of target grid in degrees using same rotation
            as source grid
        ``lambda_srce(points_lambda_srce) (64-bit REAL)``
            Lambda coords of source grid in degrees
        ``phi_srce(points_phi_srce) (64-bit REAL)``
            Phi coords of source grid in degrees
        ``t_lambda(points) (64-bit REAL)``
            Local value of target longitude

    **Outputs**
        ``weight_t_r(points) (64-bit REAL)``
            Weight applied to value at top right corner of source gridbox
        ``weight_b_l(points) (64-bit REAL)``
            Weight applied to value at bottom left corner of source gridbox
        ``weight_b_r(points) (64-bit REAL)``
            Weight applied to value at bottom right corner of source gridbox
        ``weight_t_l(points) (64-bit REAL)``
            Weight applied to value at top left corner of source gridbox

    **Notes**
        The arguments here are *all* 64-bit.


