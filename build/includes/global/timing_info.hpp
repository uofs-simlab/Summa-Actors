#pragma once
#include <chrono>
#include <optional>
#include <vector>
#include <string>

/**
 * Class to manage timing information. This allows the user to add an arbitrary amount of timing variables.
 * The timing variables are accessed through their named string and will keep a running duration of the amount 
 * of time spent through multiple calls to updateStartPoint and updateEndPoint
 */
class TimingInfo {
  private:
    std::vector<std::optional<std::chrono::time_point<
        std::chrono::high_resolution_clock>>> start;
    std::vector<std::optional<std::chrono::time_point<
        std::chrono::high_resolution_clock>>> end;
    std::vector<double> duration;
    std::vector<std::string> name_of_time_point; // the name you want for the time point (ie. reading, writing, duration)
    int num_time_points;

    std::optional<double> calculateDuration(int index);
    std::optional<int>  getIndex(std::string time_point_name);

  public:
    TimingInfo();
    ~TimingInfo();
    void addTimePoint(std::string time_point_name);
    void updateStartPoint(std::string time_point_name);
    void updateEndPoint(std::string time_point_name);
    std::optional<double> getDuration(std::string time_point_name); // returns duration in seconds

};