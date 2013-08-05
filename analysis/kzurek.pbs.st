#PBS -q flux
#PBS -l nodes=1:ppn=1,walltime=06:00:00,qos=flux
#PBS -A kzurek_flux
#PBS -N evgen
##PBS -o output1.log
#PBS -S /bin/bash
#PBS -M iankim@umich.edu
#PBS -j oe
#PBS -V

module load gsl 

echo \$PWD
cat \$PBS_NODEFILE
echo \$LD_LIBRARY_PATH
date

#mkdir -p /tmp/pipeline/"\$PBS_JOBID"


cd \$HOME/repo/src/lhc-analysis-collection/analysis

pipeline-eventgen deploy "\$PBS_JOBID" /tmp/pipeline/"\$PBS_JOBID"config1.txt 

./runProspino install /tmp/pipeline/"\$PBS_JOBID"
./$exename$ /tmp/pipeline/"\$PBS_JOBID"config1.txt /tmp/pipeline/"\$PBS_JOBID" $n1$ $n2$

pipeline-eventgen remove "\$PBS_JOBID"

# rm -rf /tmp/pipeline/"\$PBS_JOBID"



