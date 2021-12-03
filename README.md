# ABOUT #

See repo DBuddy about the goal of the larger project

DBuddy client allows for passing dbuddy commands to a server. This improves latency for larger queries and allows for low local processing and dependencies. 

# CONFIG #

For use of dbuddy_client, you will need:

* R vers >= 4.0.0
* System path with
	* ~/dbuddy/bin
	* ~/R-x.x.x/bin

This should allow you to run dbuddy commands. To see the effect of various commands, look in the text of DanWoodrich/dbuddy/lib/exec

dbuddy_client allows you to specify if you would like DML command response returned as text in console or as a log with --log . Look at dbuddy_server/lib/exec for further details on how dbuddy_client commands are 
parsed. In general, it uses the same commands as local DBuddy. 