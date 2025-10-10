#pragma once

#include "ast/Debug.h"
#include "ast/Entity.h"
#include <iostream>
#include <memory>
#include <unordered_set>
#include <vector>
#include <queue>

#if AST_DEBUG_ON

namespace Ast {

class DebugTreePrinter final {
public:
    inline void setDepth(unsigned int depth) noexcept { depth_ = depth; }
    inline unsigned int getDepth() const noexcept { return depth_; }

    void print(const std::shared_ptr<Ast::Entity>& v, const std::string& prefix) {
        std::cout << std::string(depth_, ' ') << prefix;
        v->print(std::cout);
        // std::cout << " " << v->span << "\n";
    }
private:
    unsigned int depth_{0};
};

class DebugTree final {
public:
    DebugTree() {
        nodes_.reserve(1024);
        nodes_.push_back(nullptr);
    }

    void newNode(std::shared_ptr<Ast::Entity> node) {
        node->debugId = globalDebugId_++;
        nodes_.emplace_back(std::move(node));
    }
    void printAll() {
        for (size_t i = 1; i < nodes_.size(); i++) {
            while (!depthStack_.empty()) {
                auto [debugId, depth] = depthStack_.top();
                depthStack_.pop();

                printer_.setDepth(depth);

                printer_.print(
                    nodes_[debugId],
                    std::string("[").append(std::to_string(debugId)).append("]  ")
                );
                if (depthIncrement_) {
                    printer_.setDepth(printer_.getDepth() + 1);
                    depthIncrement_ = false;
                }
            }

            if (alreadyDisplayed_.find(i) != alreadyDisplayed_.end())
                continue;
            alreadyDisplayed_.emplace(i);
            printer_.print(
                nodes_[i],
                std::string("[").append(std::to_string(i)).append("]  ")
            );
        }
    }

    void pushPrint(unsigned long debugId) {
        if (debugId == 0 || alreadyDisplayed_.find(debugId) != alreadyDisplayed_.end())
            return;
        alreadyDisplayed_.emplace(debugId);
        depthStack_.push({debugId, printer_.getDepth() + 1});
        depthIncrement_ = true;
    }
private:
    std::vector<std::shared_ptr<Ast::Entity>> nodes_;

    using DepthElem = std::pair<unsigned long, unsigned int>;
    class DepthComparator {
    public:
        bool operator() (DepthElem a, DepthElem b) {
            return a.second <= b.second && a.first > b.first;
        }
    };

    std::priority_queue<DepthElem, std::vector<DepthElem>, DepthComparator> depthStack_;
    std::unordered_set<unsigned long> alreadyDisplayed_;
    DebugTreePrinter printer_;
    unsigned long globalDebugId_{1};
    bool depthIncrement_{false};
};

// GLOBAL VARIABLE
inline DebugTree debugInfo;

}
#endif
