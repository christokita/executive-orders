For now, the data path is 

   R -> csv -> gephi -> JSON -> sigma.js page

the file you would need to emulate to cut gephi out of the loop is the

  data.json

For you to be able to view this on your local machine you need to have python installed then from the cygwin prompt

  cd /cygwin/u/projectfolder/
  python -m SimpleHTTPServer  
  
for others to be able to view it  

  python -c 'import BaseHTTPServer as bhs, SimpleHTTPServer as shs; bhs.HTTPServer(("10.10.124.105", 8888), shs.SimpleHTTPRequestHandler).serve_forever()'

replacing 10.10.124.105 with your IP address, which can be found by typing

  ipconfig
  
and spotting the IPV4 number. For example, 

   IPv4 Address. . . . . . . . . . . : 10.10.124.0
   
   
From a browser on IDA's intranet, type "10.10.124.105:8888" without the quotes and hit enter. (Replace 10.10.124.105 with your IP address)
   
