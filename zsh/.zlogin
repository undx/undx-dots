
# Execute code that does not affect the current session in the background.
{
  # zlogin: Compile the completion dump to increase startup speed.
  zcompdump="$HOME/.zcompdump"
  if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
    zcompile "$zcompdump"
  fi
} &!

archey