// (c) Michael Buro 1992-2002, licensed under the GNU Public License, version 2

/*
    (c) Michael Buro, mic@research.nj.nec.com
    NEC Research Institute
    4 Independence Way
    Princeton, NJ 08540, USA

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/


#ifndef OthClient_H
#define OthClient_H

#include "Client.H"
#include "OthClientGame.H"

class OthClient : public Client {

public:

  OthClient(const String &host, sint4 port, const String &login, const String &passwd) :
    Client("/os", host,port,login,passwd) { }

  ClientGame *new_ClientGame() const { return new OthClientGame(); }

  ClientGameType *new_ClientGameType() const { return new OthClientGameType(); }
  ClientMove     *new_ClientMove()     const { return new OthClientMove(); }

};

#endif

