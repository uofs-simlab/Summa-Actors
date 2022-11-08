#include <gtest/gtest.h>
#include </u1/kck540/Summa-Sundials/Summa-Actors/build/includes/summa_actor/client.hpp>

// Demonstrate some basic assertions.
TEST(HelloTest, BasicAssertions) {
  // Expect two strings not to be equal.
  EXPECT_STRNE("hello", "world");
  // Expect equality.
  EXPECT_EQ(7 * 6, 42);
}

