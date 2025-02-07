#!/bin/tcsh

# try to find reasonable random event related timing given the experimental
# parameters

# ---------------------------------------------------------------------------
# some experiment parameters (most can be inserted directly into the
# make_random_timing.py command)

set num_stim    = 4
set num_runs    = 8
set pre_rest    = 6     # min rest before first stim (for magnet steady state)
set post_rest   = 12    # min rest after last stim (for trailing BOLD response)
set min_rest    = 4     # minimum rest after each stimulus
set tr          = 1.2   # used in 3dDeconvolve, if not make_random_timing.py

# (options that take multiple values can also take just one if they are
# all the same, such as with this example)
#
# set stim_durs   = "2.25 2.25 2.25 2.25"
# set stim_reps   = "12 12 12 12"
# set run_lengths = "300 300 300"

set stim_durs   = 1 3 1 3 # durations of each stimulus; 1 sec. cue, 3 sec. movement
set stim_reps   = 12      # number of trials per run
set run_lengths = 300     # durations of r, seconds
set labels      = A.cue A.move B.cue B.move # stimuli labels
set A.order     = A.cue A.move # first pair of ordered stimuli
set B.order     = B.cue B.move # second pair of ordered stimuli

# ---------------------------------------------------------------------------
# execution parameters
set iterations  = 100           # number of iterations to compare
set seed        = 1234567       # initial random seed
set outdir      = stim_results  # directory that all results are under
set LCfile      = NSD_sums      # file to store norm. std. dev. sums in

# set pattern   = LC            # search pattern for LC[0], say
set pattern     = 'norm. std.'  # search pattern for normalized stdev vals


# ===========================================================================
# start the work
# ===========================================================================

# ------------------------------------------------------------
# recreate $outdir each time

if ( -d $outdir ) then
   echo "** removing output directory, $outdir ..."
   \rm -fr $outdir
endif

echo "++ creating output directory, $outdir ..."
mkdir $outdir
if ( $status ) then
   echo "failure, cannot create output directory, $outdir"
   exit
endif

# move into the output directory and begin work
cd $outdir

# create empty LC file
echo -n "" > $LCfile

echo -n "iteration (of $iterations): 0000"

# ------------------------------------------------------------
# run the test many times

foreach iter (`count -digits 4 1 $iterations`)

        # make some other random seed

        @ seed = $seed + 1


        # create randomly ordered stimulus timing files
        # (consider: -tr_locked -save_3dd_cmd tempfile)

        make_random_timing.py -num_stim $num_stim -stim_dur $stim_durs  \
                -num_runs $num_runs -run_time $run_lengths              \
                -num_reps $stim_reps -prefix stimes.$iter               \
                -pre_stim_rest $pre_rest -post_stim_rest $post_rest     \
                -min_rest $min_rest                                     \
                -stim_labels $labels                                    \
                -ordered_stimuli $A.order  -ordered_stimuli $B.order    \
                -seed $seed                                             \
                -tr $tr                                                 \
                -show_timing_stats                                      \
                -save_3dd_cmd cmd.3dd.$iter                             \
                        >& out.mrt.$iter

        # consider: sed 's/GAM/"TENT(0,15,7)"/' tempfile > cmd.3dd.$iter
        #           rm -f tempfile

        # now evaluate the stimulus timings

        tcsh cmd.3dd.$iter >& out.3dD.$iter

        # save the sum of the 3 LC values
        set nums = ( `awk -F= '/'"$pattern"'/ {print $2}' out.3dD.${iter}` )

        # make a quick ccalc command
        set sstr = $nums[1]
        foreach num ( $nums[2-] )
            set sstr = "$sstr + $num"
        end
        set num_sum = `ccalc -expr "$sstr"`

        echo -n "$num_sum = $sstr : " >> $LCfile
        echo    "iteration $iter, seed $seed"                  >> $LCfile

        echo -n "\b\b\b\b$iter"
end

echo ""
echo "done, results are in '$outdir', LC sums are in '$LCfile'"
echo consider the command: "sort -n $outdir/$LCfile | head -1"

# note that if iter 042 seems to be the best, consider these commands:
#
# cd stim_results
# set iter = 042
# timing_tool.py -multi_timing stimes.${iter}_0*                  \
#                -run_len $run_lengths -multi_stim_dur $stim_durs \
#                -multi_show_isi_stats
# tcsh cmd.3dd.$iter
# 1dplot X.xmat.1D'[6..$]'
# 1dplot sum_ideal.1D
#
# - timing_tool.py will give useful statistics regarding ISI durations
#   (should be similar to what is seen in output file out.mrt.042)
# - run cmd.3dd.$iter to regenerate that X martix (to create actual regressors)
# - the first 1dplot command will show the actual regressors
#   (note that 6 = 2*$num_runs)
# - the second will plot the sum of the regressor (an integrity check)
#   (note that sum_ideal.1D is produced by cmd.3dd.$iter, along with X.xmat.1D)
