/**
 * Copyright (C) 2023  Todd W. Fulton
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *published by the Free Software Foundation, either version 3 of the
 *License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *License along with this program.  If not, see
 *<https://www.gnu.org/licenses/>.
 **/

#pragma once

#include <algo/sort/ordering.hpp>
#include <algo/structures/begin.hpp>
#include <algo/structures/end.hpp>
#include <algo/structures/insert.hpp>

#include <memory>
#include <range/v3/functional/identity.hpp>

namespace algo
{

namespace _binary_tree
{

template <class T>
struct Node
{
    Node* left;
    Node* right;
    Node* parent;
    T value;

    template <class... Args>
    requires(std::constructible_from<T, Args...>)
    constexpr Node(Node* left, Node* right, Node* parent, Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, Args...>)
        : left{left}
        , right{right}
        , parent{parent}
        , value{std::forward<Args>(args)...}
    {
    }

    template <class... Args>
    requires(std::constructible_from<T, Args...>)
    constexpr explicit Node(Args&&... args)
        noexcept(std::is_nothrow_constructible_v<T, Args...>)
        : value{std::forward<Args>(args)...}
    {
    }

    constexpr Node() = delete;
    constexpr Node(Node const&) = delete;
    constexpr Node& operator=(Node const&) = delete;

    constexpr Node(Node&& other) noexcept(std::is_nothrow_move_constructible_v<T>)
        : left{std::exchange(other.left, nullptr)}
        , right{std::exchange(other.right, nullptr)}
        , parent{std::exchange(other.parent, nullptr)}
        , value{std::move(other.value)}
    {
    }

    constexpr ~Node() = default;
};
} // namespace _binary_tree

template <class T,
          class Relation = ordering::ascending_fn,
          class Projection = ranges::identity,
          class Allocator = std::allocator<_binary_tree::Node<T>>>
class BinaryTree
{
public:
    using allocator_type = Allocator;
    using relation_type = Relation;
    using projection_type = Projection;
    using size_type = size_t;
    using value_type = std::remove_cvref_t<T>;

private:
    using Node = _binary_tree::Node<T>;

    Node* root_{};
    size_type count_{};
    [[no_unique_address]] allocator_type alloc_;
    [[no_unique_address]] ranges::semiregular_box_t<Relation> relation_;
    [[no_unique_address]] ranges::semiregular_box_t<Projection> projection_;

    class Iterator
    {
        friend BinaryTree;

        Node* node{};
        BinaryTree* tree;

        constexpr explicit Iterator(BinaryTree* tree) noexcept
            : tree{tree}
        {
        }

        constexpr Iterator(Node* node, BinaryTree* tree) noexcept
            : node{node}
            , tree{tree}
        {
        }

        constexpr friend void swap(Iterator& a, Iterator& b) noexcept
        {
            if (&a != &b) {
                Iterator tmp{std::move(a)};
                a = std::move(b);
                b = std::move(tmp);
            }
        }

    public:
        using iterator_category = std::bidirectional_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using pointer = value_type const*;
        using reference = value_type const&;

        constexpr Iterator() noexcept = default;

        constexpr Iterator(Iterator&& other) noexcept
            : node{std::exchange(other.node, nullptr)}
            , tree{std::exchange(other.tree, nullptr)}
        {
        }

        constexpr Iterator(Iterator const&) noexcept = default;

        constexpr ~Iterator()
        {
            node = nullptr;
            tree = nullptr;
        }

        constexpr Iterator& operator=(Iterator const&) noexcept = default;
        constexpr Iterator& operator=(Iterator&& other) noexcept
        {
            if (this != &other) {
                this->tree = std::exchange(other.tree, nullptr);
                this->node = std::exchange(other.node, nullptr);
            }

            return *this;
        }

        constexpr reference operator*()
        {
            return node->value;
        }

        constexpr Iterator& operator++()
        {
            if (node->right != nullptr) {
                while (node->left != nullptr) {
                    node = node->left;
                }
            }
            else {
                Node* parent = node->parent;
                while (parent != nullptr and node == parent->right) {
                    node = parent;
                    parent = parent->parent;
                }
                node = parent;
            }
            return *this;
        }

        constexpr Iterator operator++(int) noexcept
        {
            Iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        constexpr Iterator& operator--() noexcept
        {
            if (node == nullptr) {
                // TODO: move to last node in the tree
                node = tree->root_;
                if (node != nullptr) {
                    while (node->right != nullptr) {
                        node = node->right;
                    }
                }
            }
            else if (node->left != nullptr) {
                while (node->right != nullptr) {
                    node = node->right;
                }
            }
            else {
                Node* parent = node->parent;
                while (parent != nullptr and node == parent->left) {
                    node = parent;
                    parent = parent->parent;
                }
                node = parent;
            }
            return *this;
        }

        constexpr Iterator operator--(int) noexcept
        {
            Iterator tmp = *this;
            ++(*this);
            return tmp;
        }

        friend bool operator==(Iterator const& a, Iterator const& b) noexcept
        {
            assert(a.tree == b.tree);
            return a.node == b.node;
        }
    };

public:
    using iterator = Iterator;
    using const_iterator = Iterator;

    template <ranges::range Rng>
    requires(std::constructible_from<ranges::range_value_t<Rng>>)
    constexpr explicit BinaryTree(Rng&& rng)
    {
        for (auto&& elem : std::forward<Rng>(rng)) {
            insert(std::forward<decltype(elem)>(elem));
        }
    }

    constexpr ~BinaryTree()
    {
        using AT = std::allocator_traits<allocator_type>;
        Node* node = root_;
        Node* parent = nullptr;
        while (node != nullptr) {
            if (node->left != nullptr) {
                node = node->left;
            }
            else if (node->right != nullptr) {
                node = node->right;
            }
            else {
                parent = node->parent;
                if (parent != nullptr) {
                    if (parent->left == node) {
                        parent->left = nullptr;
                    }
                    else {
                        parent->right = nullptr;
                    }
                }
                else {
                    root_ = nullptr;
                }
                AT::destroy(alloc_, node);
                AT::deallocate(alloc_, node, 1);
                --count_;
                node = parent;
            }
        }
    }

private:
    template <class... Args>
    requires(std::constructible_from<value_type, Args...>)
    constexpr friend const_iterator tag_invoke([[maybe_unused]] insert_fn tag,
                                               BinaryTree& tree,
                                               Args&&... args)
    {
        using AT = std::allocator_traits<BinaryTree::allocator_type>;
        Node* node = AT::allocate(tree.alloc_, std::forward<Args>(args)...);
        Node** insert_node = &tree.root_;
        Node* parent = nullptr;
        auto& rel = tree.relation_;
        auto& proj = tree.projection_;
        while (*insert_node != nullptr) {
            parent = *insert_node;
            if (rel(proj((*insert_node)->value, proj(node->value)))) {
                // if relation is less than
                // less
                insert_node = (*insert_node)->left;
            }
            else if (rel(proj(node->value), proj((*insert_node)->value))) {
                // if relation is less than
                // greater
                // right is less than new value, so new value must be greater
                insert_node = (*insert_node)->right;
            }
            else {
                // if relation is less than
                // equal
                // keep searching until we find not equal
                // then insert between the last equal node and the next greater node
            }
        }
        *insert_node = node;
        node->parent = parent;
        return {*insert_node, &tree};
    }

    constexpr friend const_iterator tag_invoke([[maybe_unused]] begin_fn tag,
                                               BinaryTree const& tree) noexcept
    {
        const_iterator itr{tree->root_, &tree};
        if (itr.node != nullptr) {
            while (itr.node->left != nullptr) {
                itr.node = itr.node->left;
            }
        }
        return itr;
    }

    constexpr friend const_iterator tag_invoke([[maybe_unused]] end_fn tag,
                                               BinaryTree const& tree) noexcept
    {
        return {&tree};
    }
};
} // namespace algo
