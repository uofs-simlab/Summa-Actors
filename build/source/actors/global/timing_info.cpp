#include "timing_info.hpp"
#include <chrono>
#include <algorithm>

TimingInfo::TimingInfo() {
    this->num_time_points = 0;
}

TimingInfo::~TimingInfo(){}

std::optional<double> TimingInfo::calculateDuration(int index) {
    if (!this->start[index].has_value() || !this->end[index].has_value()) {
        return {};
    } else {
        auto start = this->start[index].value();
        auto end = this->end[index].value();
        return std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();

    }
}

std::optional<int> TimingInfo::getIndex(std::string time_point_name) {

    auto itr = std::find(this->name_of_time_point.begin(), 
        this->name_of_time_point.end(),
        time_point_name);
    if (itr != this->name_of_time_point.end()) {
        return std::distance(this->name_of_time_point.begin(), itr);
    } else {
        return {};
    }
}

void TimingInfo::addTimePoint(std::string time_point_name) {
    this->name_of_time_point.push_back(time_point_name);
    this->start.push_back({});
    this->end.push_back({});
    this->duration.push_back(0.0);
    this->num_time_points++;
}

void TimingInfo::updateStartPoint(std::string time_point_name) {
    std::optional<int> index = getIndex(time_point_name);

    if (index.has_value()) {
        this->start[index.value()] = std::chrono::high_resolution_clock::now();
    }
}

void TimingInfo::updateEndPoint(std::string time_point_name) {
    std::optional<int> index = getIndex(time_point_name);
    if (index.has_value()) {
        this->end[index.value()] = std::chrono::high_resolution_clock::now();
        std::optional<double> duration = calculateDuration(index.value());
        if (duration.has_value())
            this->duration[index.value()] += duration.value();
    }
}

std::optional<double> TimingInfo::getDuration(std::string time_point_name) {
    std::optional<int> index = getIndex(time_point_name);
    if (index.has_value()) {
        double duration = this->duration[index.value()];
        duration = duration / 1000; // convert to miliseconds
        duration = duration / 1000; // convert to seconds
        return duration;
    } else {
        return {};
    }
}