#include "timing_info.hpp"

TimingInfo::TimingInfo() {
    this->num_time_points = 0;
}

TimingInfo::~TimingInfo(){}

void TimingInfo::addTimePoint(std::string time_point_name) {
    this->name_of_time_point.push_back(time_point_name);
    this->start.push_back({});
    this->end.push_back({});
    this->num_time_points++;
}