/* find nodes of max and min degree */
BEG_G {node_t mxn, mnn; int maxd = -1; int mind = 1000000;}
N { 
if (degree > maxd) 
{maxd = degree; mxn = $;} 
if (degree < mind) 
{mind = degree; mnn = $;} 
}
END_G {printf ("max degree = %d, node %s, min degree = %d, node %s\n", 
  maxd, mxn.name, mind, mnn.name)}
