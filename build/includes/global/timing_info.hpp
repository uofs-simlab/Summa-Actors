#pragma once
#include <chrono>
#include <optional>
#include <vector>

using chrono_time = std::chrono::time_point<std::chrono::system_clock>;

class TimingInfo {
    private:
        std::vector<std::optional<chrono_time>> start;
        std::vector<std::optional<chrono_time>> end;
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