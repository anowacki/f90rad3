#!/bin/bash
# Plot a rad3 file

t1=0
x1=0
X=12
Y=12

usage () {
	{
		echo "Usage: `basename $0` (options) [rad3 file without the extension]"
		echo "Plots the file using GMT"
		echo "Options:"
		echo "   -a [max amp.]  : Set maximum amplitude for color scale [scaled]"
		echo "   -b             : Batch mode: don't display plot"
		echo "   -gain [start_sample linear_gain exp_gain]"
		echo "                  : Apply time-varying gain to sample"
		echo "   -o [file]      : Save plot to file [temporary file]"
		echo "   -rmean         : Remove mean trace"
		echo "   -scale [scale] : Set horizontal scale [fills x axis]"
		echo "   -t [t0 t1]     : Plot time window [all]"
		echo "   -xlim [x0 x1]  : Distance window [all]"
		echo "   -v             : Verbose output from rad3dump"
		echo "   -x|-y [size]   : Set size of x or y axis in cm [$X cm|$Y cm]"
		echo "   -h(elp)        : Print this message"
	} > /dev/stderr
	exit 1
}

[ $# -eq 0 ] && usage

while [ "$1" ]; do
	case "$1" in
		-a) Amax="$2"; shift 2 ;;
		-b) batch=1; shift ;;
		-gain) start_samp="$2"; lin_gain="$3"; exp_gain="$4"; 
			flags="$flags -gain $start_samp $lin_gain $exp_gain"; shift 4 ;;
		-h|-help) usage 2>&1 ;;
		-rmean) flags="$flags -rmean"; shift ;;
		-o) outfile="$2"; shift 2 ;;
		-scale) xscale="$2"; yscale="$3"; shift 3 ;;
		-t) t1=$2; t2=$3; shift 3 ;;
		-xlim) x1="$2"; x2="$3"; shift 3 ;;
		-v) flags="$flags -v"; shift ;;
		-x) X="$2"; shift 2 ;;
		-y) Y="$2"; shift 2 ;;
		*) [ $# -ne 1 ] && usage || file="$1"; shift ;;
	esac
done

[ ! -r "$file.rd3" ] && { echo "Cannot read file \"$file.rd3\"" >&2; exit 1; }

GRD=$(mktemp /tmp/rad3plot.grdXXXXXX)
CPT=$(mktemp /tmp/rad3plot.cptXXXXXX)
FIG=$(mktemp /tmp/rad3plot.psXXXXXX)
trap "rm -f $FIG $GRD $CPT" EXIT

# Make grd file and get relevant info from it
rad3toncf $flags -o "$GRD" "$file"
set -- $(grdinfo "$GRD" | awk '/x_max/||/y_max/ {print $5}')
[ -z "$x2" ] && x2=$1
[ -z "$t2" ] && t2=$2

# Get maximum amplitude
[ -z "$Amax" ] &&
	read Amax dA <<< $(grdinfo "$GRD" |
		awk '/z_min/{min=$3; max=$5; if (-min > max) max=-min; print max, max/41}') &&
		echo "Max amplitude: $Amax" ||
	dA=$(awk -v a=$Amax 'BEGIN{print a/41}')
makecpt -Cpolar -T-$Amax/$Amax/$dA -D > "$CPT"
[ -n "$xscale" ] && J=x${xscale}/-${yscale}c || J=X${X}c/-${Y}c
grdimage "$GRD" -J$J -R$x1/$x2/$t1/$t2 -C"$CPT" \
	-Ba10f5:"Distance / m":/a100f10:"Time / ns":nSeW > "$FIG"
[ -n "$outfile" ] && cp "$FIG" "$outfile"
[ -z "$batch" ] && gv "$FIG" 2>/dev/null
