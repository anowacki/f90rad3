#!/bin/bash
# Plot a rad3 file

t1=0

usage () {
	{
		echo "Usage: `basename $0` (options) [rad3 file without the extension]"
		echo "Plots the file using GMT"
		echo "Options:"
		echo "   -a [max amp.] : Set maximum amplitude for color scale [scaled]"
		echo "   -gain [start_sample linear_gain exp_gain]"
		echo "                 : Apply time-varying gain to sample"
		echo "   -rmean        : Remove mean trace"
		echo "   -t [t0 t1]    : Plot time window [all]"
		echo "   -v            : Verbose output from rad3dump"
	} > /dev/stderr
	exit 1
}

[ $# -eq 0 ] && usage

while [ "$1" ]; do
	case "$1" in
		-a) Amax="$2"; shift 2 ;;
		-gain) start_samp="$2"; lin_gain="$3"; exp_gain="$4"; 
			flags="$flags -gain $start_samp $lin_gain $exp_gain"; shift 4 ;;
		-rmean) flags="$flags -rmean"; shift ;;
		-t) t1=$2; t2=$3; shift 3 ;;
		-v) flags="$flags -v"; shift ;;
		*) [ $# -ne 1 ] && usage || file="$1"; shift ;;
	esac
done

[ ! -r "$file.rd3" ] && { echo "Cannot read file \"$file.rd3\"" >&2; exit 1; }

GRD=$(mktemp /tmp/rad3plot.grdXXXXXX)
CPT=$(mktemp /tmp/rad3plot.cptXXXXXX)
FIG=$(mktemp /tmp/rad3plot.psXXXXXX)
trap "rm -f $GRD $CPT" EXIT

# Get info for creating grd file
set -- $(rad3info "$file" | awk '/delta/||/dx/||/xmax/||/tmax/ {print $3}')
delta=$1
dx=$2
xmax=$3
[ -z "$t2" ] && t2=$4

# Make netcdf grd file
rad3dump $flags "$file" | xyz2grd -R0/$xmax/$t1/$t2 -I$dx/$delta -G"$GRD"
# Get maximum amplitude
[ -z "$Amax" ] &&
	Amax=$(grdinfo "$GRD" |
		awk '/z_min/{min=$3; max=$5; if (-min > max) max=-min; print max}')
dA=$(echo "$Amax/41" | bc -l)
makecpt -Cpolar -T-$Amax/$Amax/$dA > "$CPT"
grdimage "$GRD" -JX12c/-12c -R -C"$CPT" \
	-Ba10f5:"Distance / m":/a100f90:"Time / ns":nSeW > "$FIG"
gv "$FIG" 2>/dev/null
