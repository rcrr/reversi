// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

// simple example application
//
// on how to retrieve game information take a look at fragment.C

#include "OthClient.H"
#include <unistd.h>

static bool analyse = false;

int main()
{
  OthClient client("external.nj.nec.com", 4000, "client", "clientpw");

  client.add_super_user("mic");
  client.send("who\n");

  for (;;) {

    client.io();

    if (client.state() != Client::UNDEF) {
      
      if (client.state() == Client::REMOTE_CMD) {
	
	cout << "REMOTE cmd received from '"
	     << client.remote.sender
	     << "': "
	     << client.remote.msg[0]
	     << endl;
	
	String msg = "tell " + client.tell.sender + " issued your command\n";
	client.send(msg);
	client.send(client.remote.msg[0]+"\n");
	
      } else if (client.state() == Client::INTERNAL_CMD) {
	
	if (client.internal.msg == "analyse") {
	  
	  analyse = !analyse;
	  cout << "set analyse = " << analyse << endl;
	  
	} else {
	  
	  client.send(client.internal.msg + "\n");
	}

      }
      
      client.reset();
    }
    
    usleep( 100000 );
  }
  
  return 0;
};

