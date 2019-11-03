######################################################################################################################################
Voltage: -80
Freq:   3400
Temp:  59/69 (single-core/all-cores)
  Factorizing SDF matrix of size    24046:                    [   105.597724210][   105.635223087] (plain)
  Factorizing SDF matrix of size    24046:                    [   219.279628873][    27.994042999] (blocked-768 - openmp-task-dependecies)

rcrr@acheron:~/base/prj/reversi/c$ ./build/test/bin/ut_linear_algebra -v -m perf -k 7 -p /linear_algebra/lial_perf_sdf_lapack_xedge_000_bp
/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp: 
  Reading from storage SDF matrix:                            [     1.295889799][     1.295944557]
  Factorizing SDF matrix of size    24046:                    [   230.127051869][    29.795161822]
  Computing the inverse matrix:                               [   675.596920583][   675.556097905]
  Multiplying the original random SDF matrix and its inverse: [   601.174575648][   601.221063439]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000005
    normalized_max_row_delta_modulus =         0.000000000000000027, row #  4533
    max_delta                        =         0.000000000000004014, i #  4533, j #  7277
/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp:                                [  1512.228129891] [6.7]+ OK



######################################################################################################################################
Voltage: -80
Freq:   3400
Temp:  60/68 (single-core/all-cores)
  Factorizing SDF matrix of size    24046:                    [   103.625639152][   103.626778475] (plain)
  Factorizing SDF matrix of size    24046:                    [   106.202026756][   106.204760411] (blocked)
  Factorizing SDF matrix of size    24046:                    [   230.886653363][    29.151437811] (blocked - opnenmp)
  
rcrr@acheron:~/base/prj/reversi/c$ ./build/test/bin/ut_linear_algebra -v -m perf -k 7 -p /linear_algebra/lial_perf_sdf_lapack_xedge_000_bp (openmp)
/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp: 
  Reading from storage SDF matrix:                            [     1.294336015][     1.294361482]
  Factorizing SDF matrix of size    24046:                    [   230.886653363][    29.151437811]
  Computing the inverse matrix:                               [   668.907220992][   668.846353925]
  Multiplying the original random SDF matrix and its inverse: [   597.671810838][   597.682261398]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000005
    normalized_max_row_delta_modulus =         0.000000000000000027, row #  4533
    max_delta                        =         0.000000000000004014, i #  4533, j #  7277
/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp:                                [  1502.786797921] [6.7]+ OK

######################################################################################################################################
Voltage: -50
Freq:   3200
Temp:  59/67 (single-core/all-cores)
  Factorizing SDF matrix of size    24046:                    [   110. 79739920][   110. 87243001]
  Factorizing SDF matrix of size    24046:                    [   242.633809832][    30.640529771]

rcrr@acheron:~/base/prj/reversi/c$ ./build/test/bin/ut_linear_algebra -v -m perf -k 7 -p /linear_algebra/lial_perf_sdf_lapack_xedge_000_bp (openmp)
/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp: 
  Reading from storage SDF matrix:                            [     1.367972410][     1.367988942]
  Factorizing SDF matrix of size    24046:                    [   240.717953497][    31.499705598]
  Computing the inverse matrix:                               [   721.704486608][   721.704253945]
  Multiplying the original random SDF matrix and its inverse: [   646.602392328][   646.644872415]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000005
    normalized_max_row_delta_modulus =         0.000000000000000027, row #  4533
    max_delta                        =         0.000000000000004014, i #  4533, j #  7277
/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp:                                [  1614.707811681] [6.7]+ OK

rcrr@acheron:~/base/prj/reversi/c$ ./build/test/bin/ut_linear_algebra -v -m perf -k 7 -p /linear_algebra/lial_perf_sdf_lapack_xedge_000_bp (single thread - tiles )
/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp: 
  Reading from storage SDF matrix:                            [     1.360368365][     1.360376530]
  Factorizing SDF matrix of size    24046:                    [   113. 18244257][   113. 19381527]
  Computing the inverse matrix:                               [   724.786751579][   724.862093849]
  Multiplying the original random SDF matrix and its inverse: [   645.498942091][   645.763759758]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000005
    normalized_max_row_delta_modulus =         0.000000000000000027, row #  4533
    max_delta                        =         0.000000000000004014, i #  4533, j #  7277
/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp:                                [  1488.915150027] [6.7]+ OK

rcrr@acheron:~/base/prj/reversi/c$ ./build/test/bin/ut_linear_algebra -v -m perf -k 7 -p /linear_algebra/lial_perf_sdf_lapack_edge_
/linear_algebra/lial_perf_sdf_lapack_edge_000_plain: 
  Reading from storage SDF matrix:                            [     0. 28701491][     0. 28710441]
  Factorizing SDF matrix of size     3002:                    [     0.252397132][     0.252399675]
  Computing the inverse matrix:                               [     1.509594590][     1.509606774]
  Multiplying the original random SDF matrix and its inverse: [     1.342061580][     1.342088544]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =         0.000000000000000000
    standard_deviation               =         0.000000000000000012
    normalized_max_row_delta_modulus =         0.000000000000000073, row #   273
    max_delta                        =         0.000000000000003997, i #   273, j #   273
/linear_algebra/lial_perf_sdf_lapack_edge_000_plain:                              [     3.205821473] [4.4]  OK

/linear_algebra/lial_perf_sdf_lapack_edge_000_bp: 
  Reading from storage SDF matrix:                            [     0. 26778418][     0. 26779486]
  Factorizing SDF matrix of size     3002:                    [     0.989359983][     0.133984342]
  Computing the inverse matrix:                               [     1.551440386][     1.479840988]
  Multiplying the original random SDF matrix and its inverse: [     1.362392648][     1.363031191]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000012
    normalized_max_row_delta_modulus =         0.000000000000000073, row #   273
    max_delta                        =         0.000000000000003997, i #   273, j #   273
/linear_algebra/lial_perf_sdf_lapack_edge_000_bp:                                 [     4. 33875415] [4.4]  OK

/linear_algebra/lial_perf_sdf_lapack_edge_000_t_bp: 
  Reading from storage SDF matrix:                            [     0. 27119730][     0. 27154366]
  Transposing SDF matrix.
  Factorizing SDF matrix of size     3002:                    [     0.850313220][     0.114948427]
  Computing the inverse matrix:                               [     1.537438233][     1.425674572]
  Multiplying the original random SDF matrix and its inverse: [     1.263096727][     1.263729587]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000012
    normalized_max_row_delta_modulus =         0.000000000000000073, row #   273
    max_delta                        =         0.000000000000003997, i #   273, j #   273
/linear_algebra/lial_perf_sdf_lapack_edge_000_t_bp:                               [     3.782569940] [4.4]  OK

rcrr@acheron:~/base/prj/reversi/c$ ./build/test/bin/ut_linear_algebra -v -m perf -k 7 -p /linear_algebra/lial_perf_sdf_lapack_corner_
/linear_algebra/lial_perf_sdf_lapack_corner_000_plain: 
  Reading from storage SDF matrix:                            [     0. 83901270][     0. 83902320]
  Factorizing SDF matrix of size     5156:                    [     1.222275285][     1.222285251]
  Computing the inverse matrix:                               [     7.204205777][     7.204342291]
  Multiplying the original random SDF matrix and its inverse: [     6.519683863][     6.519806206]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =         0.000000000000000000
    standard_deviation               =         0.000000000000000008
    normalized_max_row_delta_modulus =         0.000000000000000042, row #  2106
    max_delta                        =         0.000000000000002553, i #  1784, j #     8
/linear_algebra/lial_perf_sdf_lapack_corner_000_plain:                            [    15.238234328] [5.5]  OK

/linear_algebra/lial_perf_sdf_lapack_corner_000_bp: 
  Reading from storage SDF matrix:                            [     0. 70325264][     0. 70326318]
  Factorizing SDF matrix of size     5156:                    [     3.300133073][     0.433902115]
  Computing the inverse matrix:                               [     7. 90588868][     7. 15326643]
  Multiplying the original random SDF matrix and its inverse: [     6.362799780][     6.362886984]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =         0.000000000000000000
    standard_deviation               =         0.000000000000000008
    normalized_max_row_delta_modulus =         0.000000000000000037, row #  3955
    max_delta                        =         0.000000000000002519, i #  1784, j #     8
/linear_algebra/lial_perf_sdf_lapack_corner_000_bp:                               [    17. 61660088] [5.5]  OK

/linear_algebra/lial_perf_sdf_lapack_corner_000_t_bp: 
  Reading from storage SDF matrix:                            [     0. 71992733][     0. 71994574]
  Transposing SDF matrix.
  Factorizing SDF matrix of size     5156:                    [     3.730342005][     0.515423814]
  Computing the inverse matrix:                               [     7. 93463095][     6.986100029]
  Multiplying the original random SDF matrix and its inverse: [     6.298746508][     6.298849472]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =         0.000000000000000000
    standard_deviation               =         0.000000000000000008
    normalized_max_row_delta_modulus =         0.000000000000000037, row #  1784
    max_delta                        =         0.000000000000002523, i #  1784, j #     8
/linear_algebra/lial_perf_sdf_lapack_corner_000_t_bp:                             [    17.531177194] [5.5]  OK

rcrr@acheron:~/base/prj/reversi/c$ ./build/test/bin/ut_linear_algebra -v -m perf -k 7 -p /linear_algebra/lial_perf_sdf_lapack_xedge_
/linear_algebra/lial_perf_sdf_lapack_xedge_000_plain: 
  Reading from storage SDF matrix:                            [     1.388802683][     1.388841686]
  Factorizing SDF matrix of size    24046:                    [   110. 79739920][   110. 87243001]
  Computing the inverse matrix:                               [   705.202437817][   705.256259555]
  Multiplying the original random SDF matrix and its inverse: [   630.817970249][   630.830713369]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000006
    normalized_max_row_delta_modulus =         0.000000000000000027, row #  4533
    max_delta                        =         0.000000000000004037, i #  4533, j #  7277
/linear_algebra/lial_perf_sdf_lapack_xedge_000_plain:                             [  1451.761723665] [6.7]+ OK

/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp: 
  Reading from storage SDF matrix:                            [     1.357369079][     1.357400864]
  Factorizing SDF matrix of size    24046:                    [   241.924212487][    30.548026550]
  Computing the inverse matrix:                               [   702.348098558][   702.269472169]
  Multiplying the original random SDF matrix and its inverse: [   631.301451555][   631.311823049]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000005
    normalized_max_row_delta_modulus =         0.000000000000000027, row #  4533
    max_delta                        =         0.000000000000004014, i #  4533, j #  7277
/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp:                                [  1581.209128568] [6.7]+ OK

/linear_algebra/lial_perf_sdf_lapack_xedge_000_t_bp: 
  Reading from storage SDF matrix:                            [     1.361216222][     1.361255090]
  Transposing SDF matrix.
  Factorizing SDF matrix of size    24046:                    [   240. 26705480][    30.311018595]
  Computing the inverse matrix:                               [   701.603720898][   701.516289364]
  Multiplying the original random SDF matrix and its inverse: [   629.920693050][   629.933785459]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000005
    normalized_max_row_delta_modulus =         0.000000000000000027, row #  4533
    max_delta                        =         0.000000000000004014, i #  4533, j #  7277
/linear_algebra/lial_perf_sdf_lapack_xedge_000_t_bp:                              [  1581.556668073] [6.7]+ OK

rcrr@acheron:~/base/prj/reversi/c$ 

#######################
### TILED no parallel #
#######################

rcrr@acheron:~/base/prj/reversi/c$ ./build/test/bin/ut_linear_algebra -v -m perf -k 7 -p /linear_algebra/lial_perf_sdf_lapack_edge_
/linear_algebra/lial_perf_sdf_lapack_edge_000_plain: 
  Reading from storage SDF matrix:                            [     0. 25632542][     0. 25634374]
  Factorizing SDF matrix of size     3002:                    [     0.228577357][     0.228586826]
  Computing the inverse matrix:                               [     1.344043501][     1.344049351]
  Multiplying the original random SDF matrix and its inverse: [     1.207169828][     1.207195420]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =         0.000000000000000000
    standard_deviation               =         0.000000000000000012
    normalized_max_row_delta_modulus =         0.000000000000000073, row #   273
    max_delta                        =         0.000000000000003997, i #   273, j #   273
/linear_algebra/lial_perf_sdf_lapack_edge_000_plain:                              [     2.870504149] [4.4]  OK

/linear_algebra/lial_perf_sdf_lapack_edge_000_bp: 
  Reading from storage SDF matrix:                            [     0. 23624247][     0. 23626162]
  Factorizing SDF matrix of size     3002:                    [     0.235993743][     0.235995415]
  Computing the inverse matrix:                               [     1.334441008][     1.334481540]
  Multiplying the original random SDF matrix and its inverse: [     1.202608434][     1.202623042]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000012
    normalized_max_row_delta_modulus =         0.000000000000000073, row #   273
    max_delta                        =         0.000000000000003997, i #   273, j #   273
/linear_algebra/lial_perf_sdf_lapack_edge_000_bp:                                 [     2.861336205] [4.4]  OK

/linear_algebra/lial_perf_sdf_lapack_edge_000_t_bp: 
  Reading from storage SDF matrix:                            [     0. 23624628][     0. 23625929]
  Transposing SDF matrix.
  Factorizing SDF matrix of size     3002:                    [     0.224442413][     0.224460090]
  Computing the inverse matrix:                               [     1.336904504][     1.336926354]
  Multiplying the original random SDF matrix and its inverse: [     1.202275905][     1.202299362]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000012
    normalized_max_row_delta_modulus =         0.000000000000000073, row #   273
    max_delta                        =         0.000000000000003997, i #   273, j #   273
/linear_algebra/lial_perf_sdf_lapack_edge_000_t_bp:                               [     2.878961350] [4.4]  OK

rcrr@acheron:~/base/prj/reversi/c$ ./build/test/bin/ut_linear_algebra -v -m perf -k 7 -p /linear_algebra/lial_perf_sdf_lapack_corner_
/linear_algebra/lial_perf_sdf_lapack_corner_000_plain: 
  Reading from storage SDF matrix:                            [     0. 66133934][     0. 66135097]
  Factorizing SDF matrix of size     5156:                    [     1. 82976039][     1. 83059369]
  Computing the inverse matrix:                               [     6.411478020][     6.411836087]
  Multiplying the original random SDF matrix and its inverse: [     5.788371320][     5.788713497]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =         0.000000000000000000
    standard_deviation               =         0.000000000000000008
    normalized_max_row_delta_modulus =         0.000000000000000042, row #  2106
    max_delta                        =         0.000000000000002553, i #  1784, j #     8
/linear_algebra/lial_perf_sdf_lapack_corner_000_plain:                            [    13.534650156] [5.5]  OK

/linear_algebra/lial_perf_sdf_lapack_corner_000_bp: 
  Reading from storage SDF matrix:                            [     0. 64527631][     0. 64528686]
  Factorizing SDF matrix of size     5156:                    [     1. 92556832][     1. 92572898]
  Computing the inverse matrix:                               [     6.432054241][     6.433210806]
  Multiplying the original random SDF matrix and its inverse: [     5.951655796][     5.952711320]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =         0.000000000000000000
    standard_deviation               =         0.000000000000000008
    normalized_max_row_delta_modulus =         0.000000000000000037, row #  3955
    max_delta                        =         0.000000000000002519, i #  1784, j #     8
/linear_algebra/lial_perf_sdf_lapack_corner_000_bp:                               [    13.727420585] [5.5]  OK

/linear_algebra/lial_perf_sdf_lapack_corner_000_t_bp: 
  Reading from storage SDF matrix:                            [     0. 64782062][     0. 64843146]
  Transposing SDF matrix.
  Factorizing SDF matrix of size     5156:                    [     1. 57906587][     1. 58006290]
  Computing the inverse matrix:                               [     6.445231056][     6.446105205]
  Multiplying the original random SDF matrix and its inverse: [     5.767722997][     5.768099486]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =         0.000000000000000000
    standard_deviation               =         0.000000000000000008
    normalized_max_row_delta_modulus =         0.000000000000000037, row #  1784
    max_delta                        =         0.000000000000002523, i #  1784, j #     8
/linear_algebra/lial_perf_sdf_lapack_corner_000_t_bp:                             [    13.631079258] [5.5]  OK

rcrr@acheron:~/base/prj/reversi/c$ ./build/test/bin/ut_linear_algebra -v -m perf -k 7 -p /linear_algebra/lial_perf_sdf_lapack_xedge_
/linear_algebra/lial_perf_sdf_lapack_xedge_000_plain: 
  Reading from storage SDF matrix:                            [     1.233693930][     1.233770733]
  Factorizing SDF matrix of size    24046:                    [    98.490370027][    98.493579283]
  Computing the inverse matrix:                               [   635.507785284][   635.527908683]
  Multiplying the original random SDF matrix and its inverse: [   563.299272828][   563.306907074]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000006
    normalized_max_row_delta_modulus =         0.000000000000000027, row #  4533
    max_delta                        =         0.000000000000004037, i #  4533, j #  7277
/linear_algebra/lial_perf_sdf_lapack_xedge_000_plain:                             [  1302.349935551] [6.7]+ OK

/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp: 
  Reading from storage SDF matrix:                            [     1.228209095][     1.228236596]
  Factorizing SDF matrix of size    24046:                    [   101.238647964][   101.240098449]
  Computing the inverse matrix:                               [   635.253749209][   635.262192935]
  Multiplying the original random SDF matrix and its inverse: [   559.184438987][   559.191545278]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000005
    normalized_max_row_delta_modulus =         0.000000000000000027, row #  4533
    max_delta                        =         0.000000000000004014, i #  4533, j #  7277
/linear_algebra/lial_perf_sdf_lapack_xedge_000_bp:                                [  1300.721786892] [6.7]+ OK

/linear_algebra/lial_perf_sdf_lapack_xedge_000_t_bp: 
  Reading from storage SDF matrix:                            [     1.227793631][     1.227837554]
  Transposing SDF matrix.
  Factorizing SDF matrix of size    24046:                    [    98. 58590273][    98. 64600819]
  Computing the inverse matrix:                               [   648.257154523][   648.276852288]
  Multiplying the original random SDF matrix and its inverse: [   567.963858776][   567.986483516]
  Error matrix (deviations from the identity matrix) KPI:
    mean                             =        -0.000000000000000000
    standard_deviation               =         0.000000000000000005
    normalized_max_row_delta_modulus =         0.000000000000000027, row #  4533
    max_delta                        =         0.000000000000004014, i #  4533, j #  7277
/linear_algebra/lial_perf_sdf_lapack_xedge_000_t_bp:                              [  1323.345323897] [6.7]+ OK
