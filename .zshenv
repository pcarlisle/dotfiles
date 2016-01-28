export PATH=${HOME}/bin:/usr/local/bin:/opt/bin:/opt/local/bin:/opt/local/sbin:/bin:/usr/bin:/usr/local/sbin:/usr/games/bin
export PATH="$HOME/.rbenv/bin:$PATH"

export RUBYLIB=''
export WORKDIR='/home/patrick/work'
export FACTER_LOCATION="file://$WORKDIR/facter"
export PUPPET_LOCATION="file://$WORKDIR/puppet"

# Use local beaker
export RUBYLIB=${WORKDIR}/beaker/lib:${RUBYLIB}
export PATH=${PATH}:${WORKDIR}/beaker/bin
