#include "test.hpp"
#include "client.hpp"
#include <vector>
#include "caf/all.hpp"

#define IS_TRUE(x) { if (!(x)) std::cout << __FUNCTION__ << " failed on line " << __LINE__ << std::endl; }


void TEST_CLIENT() {
    std::cout << "Testing Client\n";
    // Create 5 Clients
    int lost_node_threshold = 3;
    
    Client_Container* client_container = new Client_Container(lost_node_threshold);
    
    caf::actor dummy1;
    std::string dummy_host1 = "dummy1";
    caf::actor dummy2;
    std::string dummy_host2 = "dummy2";
    caf::actor dummy3;
    std::string dummy_host3 = "dummy3";
    caf::actor dummy4;
    std::string dummy_host4 = "dummy4";
    caf::actor dummy5;
    std::string dummy_host5 = "dummy5";
    client_container->addClient(dummy1, dummy_host1);
    client_container->addClient(dummy2, dummy_host2);
    client_container->addClient(dummy3, dummy_host3);
    client_container->addClient(dummy4, dummy_host4);
    client_container->addClient(dummy5, dummy_host5);
    
    client_container->incrementLostPotential(3);
    client_container->incrementLostPotential(3);

    IS_TRUE(!client_container->checkForLostClients());
    
    client_container->incrementLostPotential(3);
    client_container->incrementLostPotential(3);

    IS_TRUE(!client_container->checkForLostClients());

    std::cout << "********************************************\n";
    std::cout << client_container->connectedClientsToString();
    std::cout << "********************************************\n";
    std::cout << client_container->lostClientsToString();


}