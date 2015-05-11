with import <nixpkgs> {};

stdenv.mkDerivation rec { 
  name = "MCRun_ttbarheft";
  src = ./temp2;
  buildInputs = [ git heavyhiggsEnv ];
  buildCommand = ''
    git clone /home/wavewave/repo/src/lhc-analysis-collection
    cd lhc-analysis-collection/heavyhiggs
    cat > compile.sh <<EOF 
    cabal --config-file=/home/wavewave/.cabal/config sandbox init
    cabal --config-file=/home/wavewave/.cabal/config sandbox add-source ..
    HOME=$(pwd) cabal --config-file=/home/wavewave/.cabal/config install lhc-analysis-collection 
    cabal --config-file=/home/wavewave/.cabal/config exec ghc MCRun_ttbarheft.hs
    EOF
    chmod u+x compile.sh
    load-env-heavyhiggs-project ./compile.sh
    mkdir -p $out/bin/internal
    cat > MCRun_ttbarheft.sh <<EOF 
    #!${bash}/bin/bash
    source ${heavyhiggsEnv}/dev-envs/heavyhiggs-project
    $out/bin/internal/MCRun_ttbarheft
    EOF
    chmod u+x MCRun_ttbarheft.sh 
    cp -a MCRun_ttbarheft $out/bin/internal/
    cp -a MCRun_ttbarheft.sh $out/bin/
  '';
}

